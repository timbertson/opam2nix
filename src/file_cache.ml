open Lwt

exception Download_failed of string

let unsafe_chars = Str.regexp "[^-a-zA-Z0-9_.]+"
let safe str = Str.global_replace unsafe_chars "-" str

let fetch ~dest url =
	Curl.global_init Curl.CURLINIT_GLOBALALL;
	let errbuf = ref "" in
	Printf.eprintf " [ downloading %s ]\n" url;
	flush stderr;
	begin
		try
			let connection = Curl.init () in
			Curl.set_errorbuffer connection errbuf;
			let capath = try Some (Unix.getenv "CURL_CA_BUNDLE") with Not_found -> None in
			capath |> Option.may (Curl.set_cainfo connection);
			Curl.set_writefunction connection (fun s -> output_string dest s; String.length s);
			Curl.set_followlocation connection true;
			Curl.set_url connection url;
			Curl.perform connection;
			Curl.cleanup connection
		with Curl.CurlException (reason, code, str) ->
			Printf.eprintf "Curl error: %s\n" !errbuf;
			raise (Download_failed url)
	end;
	Curl.global_cleanup ();
	close_out dest

class cache ~max_age dir = object (self)
 method download (url:string) =
	let path = safe url in
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
			fetch ~dest:(open_out (path ^ "~")) url;
			rename (path ^ "~") path;
			path
end
