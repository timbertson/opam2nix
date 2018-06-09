module JSON = Yojson.Basic

module Cache = struct
	include Map.Make(String)
end
module StringSet = Set.Make(String)

type key = string
type nix_digest = [ `sha256 of string ]
type opam_digest = [ `md5 of string ]

type state = {
	digests: nix_digest Cache.t;
	active: StringSet.t;
}
type t = state ref

type value_partial = {
	val_type: string option;
	val_digest: string option;
}
let empty_partial = { val_type = None; val_digest = None }

let value_of_json = function
	| `Assoc properties -> (
		let {val_type; val_digest} = properties |> List.fold_left (fun partial item ->
			match item with
				| ("type", `String t) -> { partial with val_type = Some t }
				| ("digest", `String t) -> { partial with val_digest = Some t }
				| (other, _) -> (
					Printf.eprintf "warn: skipping unknown key: %s" other;
					partial
				)
		) empty_partial in
		match (val_type, val_digest) with
			| (Some "sha256", Some digest) | (None, Some digest) ->
				(* assume sha256 *)
				`sha256 digest
			| other -> failwith "Unknown digest value"
	)
	| other -> failwith "Expected digest value to be an object"

let json_of_nix_digest = function
	| `sha256 digest -> `Assoc [ "digest", `String digest ]

let key_of_opam_digest = function
	| `md5 digest -> "md5:" ^ digest

let opam_digest_of_key key =
	match String.split_on_char ':' key with
		| ["md5"; digest] -> `md5 digest
		| _ -> failwith ("Can't parse opam digest: " ^ key)

let json_of_cache cache =
	let properties = Cache.fold (fun key value properties ->
		(key, json_of_nix_digest value) :: properties
	) cache [] in
	`Assoc properties

let cache_of_json = function
	| `Assoc properties ->
		properties |> List.fold_left (fun map (key, value) ->
			Cache.add key (value_of_json value) map
		) Cache.empty
	| _ -> failwith "Invalid JSON mapping; expeced toplevel object"

let load path: nix_digest Cache.t =
	JSON.from_file path |> cache_of_json

let try_load path: t =
	let digests = try load path
		with Unix.(Unix_error(ENOENT, _, _)) -> Cache.empty in
	ref { digests; active = StringSet.empty }

let save cache path =
	let tmp = path ^ ".tmp" in
	json_of_cache cache |> JSON.to_file ~std:true tmp;
	Unix.rename path tmp

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

let check_digest (opam_digest:opam_digest) path =
	match opam_digest with
		| `md5 expected ->
			let actual = (md5_of_path path) in
			if (String.equal actual expected) then
				()
			else
				failwith ("md5 " ^ actual ^ " did not match expected: " ^ expected)

let add url opam_digest cache : nix_digest =
	let key = key_of_opam_digest opam_digest in
	let digests = (!cache).digests in
	let active = StringSet.add key !cache.active in
	if Cache.mem key digests then (
		cache := { !cache with active };
		Cache.find key digests
	) else (
		let (dest, dest_channel) = Filename.open_temp_file "opam2nix" "archive" in
		File_cache.fetch dest_channel url;
		let () = check_digest opam_digest dest in
		let nix_digest = `sha256 (sha256_of_path dest) in
		cache := {
			digests = Cache.add key nix_digest digests;
			active;
		};
		nix_digest
	)

