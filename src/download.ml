type error = [ `download_failed of string ]
let string_of_error (`download_failed desc) =
	"Download failed: " ^ desc

let fetch ~dest url : (unit, [> error] ) Result.t =
	Curl.global_init Curl.CURLINIT_GLOBALALL;
	let errbuf = ref "" in
	Printf.eprintf " [ downloading %s ]\n" url;
	flush stderr;
	let result = (
		try
			let connection = Curl.init () in
			Curl.set_errorbuffer connection errbuf;
			let capath = try Some (Unix.getenv "CURL_CA_BUNDLE") with Not_found -> None in
			capath |> Option.may (Curl.set_cainfo connection);
			Curl.set_writefunction connection (fun s -> output_string dest s; String.length s);
			Curl.set_followlocation connection true;
			Curl.set_url connection url;
			Curl.perform connection;
			Curl.cleanup connection;
			Ok ()
		with Curl.CurlException (_reason, _code, _str) ->
			Printf.eprintf "Curl error: %s\n" !errbuf;
			Error (`download_failed url)
	) in
	Curl.global_cleanup ();
	close_out dest;
	result
