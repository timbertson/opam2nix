open Lwt
(* type cache_entry = { *)
(* 	cache_mtime : int option; *)
(* 	cache_path : string; *)
(* } *)

let unsafe_chars = Str.regexp "[^-a-zA-Z0-9_.]+"
let safe str = Str.global_replace unsafe_chars "-" str

let fetch ~dest url =
	let get url = 
		let dest = open_out dest in
		Curl.global_init Curl.CURLINIT_GLOBALALL;
		let errbuf = ref "" in
		Printf.eprintf " [ downloading %s ]\n" url;
		flush stderr;
		begin
			try
				let connection = Curl.init () in
				Curl.set_errorbuffer connection errbuf;
				Curl.set_writefunction connection (fun s -> output_string dest s; String.length s);
				Curl.set_followlocation connection true;
				Curl.set_url connection url;
				Curl.perform connection;
				Curl.cleanup connection
			with Curl.CurlException (reason, code, str) as e ->
				Printf.eprintf "Curl error: %s\n" !errbuf;
				raise e
		end;
		Curl.global_cleanup ()
	in

	match url with
		| `http url -> get url
		| `git addr -> failwith "todo: nix-prefetch-git?"

(* let fetch ~dest url = *)
(* 	let open Cohttp in *)
(* 	let open Cohttp_lwt_unix in *)
(*  *)
(* 	let rec get url = *)
(* 		Client.get url >>= fun (resp, body) -> *)
(* 		match resp |> Response.status with *)
(* 			| `Temporary_redirect *)
(* 			| `Moved_permanently -> *)
(* 				let newloc = resp |> Response.headers |> Cohttp.Header.get_location in *)
(* 				get (match newloc with *)
(* 					| Some loc -> loc *)
(* 					| None -> failwith "no redirect location") *)
(* 			| `OK -> *)
(* 					let open Lwt_io in *)
(* 					with_file Output dest (fun file -> *)
(* 						body |> Cohttp_lwt_body.to_stream |> Lwt_stream.iter_s (fun data -> *)
(* 							Lwt_io.write file data *)
(* 						) *)
(* 					) *)
(* 			| status -> *)
(* 				failwith (Printf.sprintf *)
(* 					"Failed to download %s: %s" *)
(* 					(Cohttp.Code.string_of_status status) *)
(* 					(Uri.to_string url)) *)
(* 	in *)
(*  *)
(* 	match url with *)
(* 		| `http url -> get (Uri.of_string url) *)
(* 		| `git addr -> failwith "todo: nix-prefetch-git?" *)

class cache ~max_age dir = object (self)
 method download (url:Opam_metadata.url) =
	let path = safe (Opam_metadata.string_of_url url) in
	let path = Filename.concat dir path in
	let open Unix in
	let stat =
		try Some (stat path)
		with Unix_error(ENOENT, _, _) -> None
	in
	(* XXX rm_rf in case old path is a directory *)
	let oldest = Unix.time () -. (float_of_int max_age) in
	match stat with
		| Some stat when stat.st_mtime > oldest -> path
		| _ ->
			fetch ~dest:(path ^ "~") url;
			rename (path ^ "~") path;
			path
end
