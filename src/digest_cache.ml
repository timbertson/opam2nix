module JSON = Yojson.Basic
open Util

module Cache = struct
	include Map.Make(String)
end
module StringSet = Set.Make(String)

type key = string
type nix_digest = [
	| `sha256 of string
	| `checksum_mismatch of string
]
type checksum_result = [
	| `ok
	| `checksum_mismatch of string
]
type opam_digest = [ `md5 of string ]

type state = {
	digests: nix_digest Cache.t;
	active: StringSet.t;
	path: string option;
}
type t = state ref

type value_partial = {
	val_type: string option;
	val_digest: string option;
	val_error: string option;
}
let empty_partial = { val_type = None; val_digest = None; val_error = None }

let value_of_json : JSON.json -> nix_digest option = function
	| `Assoc properties -> (
		let {val_type; val_digest; val_error} = properties |> List.fold_left (fun partial item ->
			match item with
				| ("type", `String t) -> { partial with val_type = Some t }
				| ("digest", `String t) -> { partial with val_digest = Some t }
				| ("error", `String t) -> { partial with val_error = Some t }
				| (other, _) -> (
					Printf.eprintf "warn: skipping unknown key: %s\n" other;
					partial
				)
		) empty_partial in
		match (val_type, val_digest, val_error) with
			| (Some "sha256", Some digest, None) | (None, Some digest, None) ->
				(* assume sha256 *)
				Some (`sha256 digest)
			| (Some "checksum_mismatch", _, Some error) -> Some (`checksum_mismatch error)
			| other -> (
				Printf.eprintf "Unknown digest value; ignoring: %s\n" (JSON.to_string (`Assoc properties));
				None
			)
	)
	| other -> failwith "Expected digest value to be an object"

let json_of_nix_digest = function
	| `sha256 digest -> `Assoc [ "digest", `String digest ]
	| `checksum_mismatch desc -> `Assoc [
		"type", `String "checksum_mismatch";
		"error", `String desc;
	]

let key_of_opam_digest = function
	| `md5 digest -> "md5:" ^ digest

let opam_digest_of_key key =
	match String.split_on_char ':' key with
		| ["md5"; digest] -> `md5 digest
		| _ -> failwith ("Can't parse opam digest: " ^ key)

let json_of_cache (cache: nix_digest Cache.t) : JSON.json =
	let sorted_bindings = Cache.bindings cache
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
	| _ -> failwith "Invalid JSON mapping; expeced toplevel object"

let load path: nix_digest Cache.t =
	JSON.from_file path |> cache_of_json

let try_load path: t =
	let digests = if Sys.file_exists path then load path else Cache.empty in
	ref { digests; active = StringSet.empty; path = Some path }

let ephemeral = ref { digests = Cache.empty; active = StringSet.empty; path = None }

let save cache =
	!cache.path |> Option.may (fun path ->
		let open Unix in
		try access path [W_OK]
		with
			(* If we have a readonly path, it's probably in the nix store.
			 * Don't bother trying to write it now or in the future *)
			| Unix_error (Unix.ENOENT, _, _) -> ()
			| Unix_error (Unix.EACCES, _, _) -> (
				Printf.eprintf "Note: Digest map file is not writeable; any updates will be lost\n";
				cache := { !cache with path = None }
			)
	);
	!cache.path |> Option.may (fun path ->
		let tmp = path ^ ".tmp" in
		let json = json_of_cache !cache.digests in
		let chan = open_out tmp in
		JSON.pretty_to_channel ~std:true chan json;
		flush chan;
		close_out chan;
		Unix.rename tmp path
	)

let process_lines ~desc cmd =
	let open Lwt in
	Lwt_process.with_process_in ("", cmd) (fun proc ->
		proc#stdout |> Lwt_io.read_lines |> Lwt_stream.to_list >>= fun lines ->
		proc#close >>= fun status ->
		let open Unix in
		match status with
			| WEXITED 0 -> return lines
			| _ -> failwith (desc ^ " failed")
	)

let sha256_of_path p =
	let open Lwt in
	process_lines
		~desc:"nix-hash"
		[|"nix-hash"; "--base32"; "--flat"; "--type";"sha256"; p|]
	>>= (function
		| [line] -> return line
		| lines -> failwith ("Unexpected nix-hash output:\n" ^ (String.concat "\n" lines))
	) |> Lwt_main.run

let md5_of_path p =
	let open Lwt in
	process_lines ~desc:"md5sum" [|"md5sum"; p|]
	>>= (function
		| [line] -> return (List.hd (String.split_on_char ' ' line))
		| lines -> failwith ("Unexpected md5sum output:\n" ^ (String.concat "\n" lines))
	) |> Lwt_main.run

let check_digest (opam_digest:opam_digest) path: checksum_result =
	match opam_digest with
		| `md5 expected ->
			let actual = (md5_of_path path) in
			if String.equal actual expected then `ok
			else `checksum_mismatch ("md5 " ^ actual ^ " did not match expected: " ^ expected)

let exists opam_digest cache : bool =
	let key = key_of_opam_digest opam_digest in
	Cache.mem key !cache.digests

let add url opam_digest cache : nix_digest =
	let key = key_of_opam_digest opam_digest in
	let digests = !cache.digests in
	let active = StringSet.add key !cache.active in
	if Cache.mem key digests then (
		cache := { !cache with active };
		Cache.find key digests
	) else (
		let (dest, dest_channel) = Filename.open_temp_file "opam2nix" "archive" in
		Download.fetch dest_channel url;
		let nix_digest = match check_digest opam_digest dest with
			| `ok -> `sha256 (sha256_of_path dest)
			| `checksum_mismatch desc -> `checksum_mismatch desc
		in
		cache := {
			digests = Cache.add key nix_digest digests;
			active;
			path = !cache.path;
		};
		save cache;
		nix_digest
	)

let gc cache =
	let { digests; active } = !cache in
	let is_active key _ = StringSet.mem key active in
	let active_digests = Cache.filter is_active digests in
	cache := { !cache with digests = active_digests }
