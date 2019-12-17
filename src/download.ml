type error = [ `download_failed of string ]
let string_of_error (`download_failed desc) =
	"Download failed: " ^ desc

module Ctx : sig
	type t
	val init : unit -> t
	val destroy : t -> unit
end = struct
	type t = unit
	let init () = Curl.global_init Curl.CURLINIT_GLOBALALL
	let destroy () = Curl.global_cleanup ()
end

let fetch (_: Ctx.t) ~dest url : (unit, [> error] ) Result.t Lwt.t =
	Printf.eprintf " [ downloading %s ]\n" url;
	flush stderr;
	let connection = Curl.init () in
	let capath = try Some (Unix.getenv "CURL_CA_BUNDLE") with Not_found -> None in
	capath |> Option.may (Curl.set_cainfo connection);
	Curl.set_writefunction connection (fun s -> output_string dest s; String.length s);
	Curl.set_followlocation connection true;
	Curl.set_url connection url;
	(Lwt.finalize
		(fun () -> Curl_lwt.perform connection)
		(fun () -> Curl.cleanup connection; close_out dest; Lwt.return_unit)
	) |> Lwt.map (function
		| Curl.CURLE_OK -> Ok ()
		| err -> (
			Printf.eprintf "Curl error: %s\n" (Curl.strerror err);
			Error (`download_failed url)
		)
	)
