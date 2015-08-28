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
		Curl.global_cleanup ();
		close_out dest
	in

	(* let clone addr = *)
	(* 	let addr = Opam_metadata.concat_address addr in *)
	(* 	prerr_endline ("Cloning: " ^ addr); *)
	(* 	let open Lwt in *)
	(* 	let lines = Lwt_process.with_process_in ("", [|"nix-prefetch-git"; addr|]) (fun proc -> *)
	(* 		lwt lines = proc#stdout |> Lwt_io.read_lines |> Lwt_stream.to_list in *)
	(* 		lwt status = proc#close in *)
	(* 		let open Unix in *)
	(* 		match status with *)
	(* 			| WEXITED 0 -> return lines *)
	(* 			| _ -> failwith "nix-prefetch-git failed" *)
	(* 	) |> Lwt_main.run in *)
	(* 	let nix_path = match lines with *)
	(* 		| [line] -> line *)
	(* 		| _ -> failwith ("Unexpected nix-prefetch-git output:\n" ^ (String.concat "\n" lines)) *)
	(* 	in *)
	(* 	let tmp = dest ^ ".tmp" in *)
	(* 	let oc = open_out tmp in *)
	(* 	output_string oc (nix_path ^ "\n"); *)
	(* 	close_out oc; *)
	(* 	Unix.rename tmp dest *)
	(* in *)

	match url with
		| `http url -> get url
		| `git addr -> failwith
			("Note: git dependencies not yet supported (trying to clone "
				^(Opam_metadata.concat_address addr)^")")

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
