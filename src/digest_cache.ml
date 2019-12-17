module JSON = Yojson.Basic
open Util

module Cache = struct
	include Map.Make(String)
end
module StringSet = Set.Make(String)

type key = string
type nix_digest = [
	| `sha256 of string
]
type checksum_mismatch = [ `checksum_mismatch of string ]
type error = [
	| checksum_mismatch
	| Download.error
]
let string_of_checksum_mismatch (`checksum_mismatch e) = "Checksum mismatch: " ^ e
let string_of_error : error -> string = function
	| `checksum_mismatch _ as e -> string_of_checksum_mismatch e
	| `download_failed _ as e -> Download.string_of_error e
type opam_digest = OpamHash.t

type state = {
	digests: nix_digest Cache.t;
	path: string;
	download_ctx: Download.Ctx.t option; (* automatically populated on `fetch` and discarded on save *)
}
type t = state ref

type value_partial = {
	val_type: string option;
	val_digest: string option;
}
let empty_partial = { val_type = None; val_digest = None }

let value_of_json : JSON.t -> nix_digest option = function
	| `Assoc properties -> (
		let {val_type; val_digest} = properties |> List.fold_left (fun partial item ->
			match item with
				| ("type", `String t) -> { partial with val_type = Some t }
				| ("digest", `String t) -> { partial with val_digest = Some t }
				| (other, _) -> (
					Printf.eprintf "warn: skipping unknown key: %s\n" other;
					partial
				)
		) empty_partial in
		match (val_type, val_digest) with
			| (Some "sha256", Some digest) | (None, Some digest) ->
				(* assume sha256 *)
				Some (`sha256 digest)
			| _other -> (
				Printf.eprintf "Unknown digest value; ignoring: %s\n" (JSON.to_string (`Assoc properties));
				None
			)
	)
	| other -> failwith ("Expected digest value to be an object, got " ^ (JSON.to_string other))

let json_of_nix_digest (`sha256 digest) = `Assoc [ "digest", `String digest ]

let key_of_opam_digest digest =
	(digest |> OpamHash.kind |> OpamHash.string_of_kind |> String.lowercase_ascii)
		^ ":" ^ (digest |> OpamHash.contents)

let opam_digest_of_key key =
	match String.split_on_char ':' key with
		| ["md5"; digest] -> OpamHash.md5 digest
		| ["sha256"; digest] -> OpamHash.md5 digest
		| ["sha512"; digest] -> OpamHash.md5 digest
		| _ -> failwith ("Can't parse opam digest: " ^ key)

let json_of_cache (cache: nix_digest Cache.t) : JSON.t =
	let sorted_bindings : (string * nix_digest) list = Cache.bindings cache
		|> List.sort (fun (a, _) (b, _) -> String.compare a b)
		|> List.rev in
	
	let properties = List.fold_left (fun properties (key, value) ->
		(key, json_of_nix_digest value) :: properties
	) [] sorted_bindings
	in
	`Assoc properties

let cache_of_json = function
	| `Assoc properties ->
		properties |> List.fold_left (fun map (key, value) ->
			value_of_json value |> Option.map (fun value ->
				Cache.add key value map
			) |> Option.default map
		) Cache.empty
	| _ -> failwith "Invalid JSON mapping; expected toplevel object"

let load path: nix_digest Cache.t =
	JSON.from_file path |> cache_of_json

let try_load path: t =
	let digests = if Sys.file_exists path then load path else Cache.empty in
	ref { digests; path = path; download_ctx = None }

(* TODO make this LWT *)
let save cache =
	!cache.download_ctx |> Option.may (fun ctx ->
		Download.Ctx.destroy ctx;
		cache := { !cache with download_ctx = None }
	);
	let path = !cache.path in
	let tmp = path ^ ".tmp" in
	let json = json_of_cache !cache.digests in
	let chan = open_out tmp in
	JSON.pretty_to_channel ~std:true chan json;
	flush chan;
	close_out chan;
	Unix.rename tmp path

let sha256_of_path p =
	let open Lwt in
	Cmd.run_exn Cmd.exec_r ~block:(fun proc ->
		proc#stdout |> Lwt_io.read_lines |> Lwt_stream.to_list
	) ["nix-hash"; "--base32"; "--flat"; "--type";"sha256"; p]
	>>= (function
		| [line] -> return line
		| lines -> failwith ("Unexpected nix-hash output:\n" ^ (String.concat "\n" lines))
	) |> Lwt_main.run

let check_digests (opam_digest:opam_digest list) path: (unit, [> checksum_mismatch]) Result.t =
	assert (opam_digest <> []);
	let rec check = function
		| [] -> Ok ()
		| digest :: digests -> (
			match OpamHash.mismatch path digest with
				| Some actual -> Error (`checksum_mismatch (
					(OpamHash.to_string actual)
					^ " did not match expected: "
					^ (OpamHash.to_string digest)
				))
				| None -> check digests
		)
	in
	check opam_digest

let exists opam_digests cache : bool =
	let keys = List.map key_of_opam_digest opam_digests in
	List.exists (fun key -> Cache.mem key !cache.digests) keys

let add_custom cache ~(keys:string list) (block: unit -> (nix_digest,error) Result.t Lwt.t) : (nix_digest, error) Result.t Lwt.t =
	let digests = !cache.digests in
	let rec find_first = function
		| [] -> None
		| key::keys -> (
				try Some (Cache.find key digests)
				with Not_found -> find_first keys
	) in
	let update_cache value =
		cache := { !cache with
			digests = List.fold_left (fun map key -> Cache.add key value map) digests keys;
		}
	in

	match find_first keys with
		| Some value -> Lwt.return (Ok value)
		| None -> (
			block () |> Lwt.map (fun result -> result
				|> Result.tap update_cache
			)
		)

let ensure_ctx cache =
	!cache.download_ctx |> Option.default_fn (fun () ->
		let ctx = Download.Ctx.init () in
		cache := { !cache with download_ctx = Some ctx };
		ctx
	)

let add url opam_digests cache : (nix_digest, error) Result.t Lwt.t =
	let keys = List.map key_of_opam_digest opam_digests in
	(* TODO limit concurrent downloads? with lwt_mutex? *)
	add_custom cache ~keys (fun () ->
		let (dest, dest_channel) = Filename.open_temp_file "opam2nix" "archive" in
		(* TODO async downloads *)
		let ctx = ensure_ctx cache in
		Download.fetch ctx ~dest:dest_channel url |> Lwt.map (fun result ->
			result |> Result.bind (fun () ->
				check_digests opam_digests dest
			) |> Result.map (fun () ->
				`sha256 (sha256_of_path dest)
			)
		)
	)
