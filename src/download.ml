open Lwt

exception Download_failed of string

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
