type error = [ `download_failed of string ]
let string_of_error (`download_failed desc) =
	"Download failed: " ^ desc

let max_concurrency = 10

module Ctx = struct
	type t = unit Lwt_pool.t
	let init () =
		Curl.global_init Curl.CURLINIT_GLOBALALL;
		(* Pool is used only for concurrency limiting,
		 * it doesn't actually manage resources *)
		Lwt_pool.create max_concurrency
			~dispose:(fun () -> Lwt.return_unit)
			(fun () -> Lwt.return_unit)

	let destroy t =
		Lwt_main.run (Lwt_pool.clear t);
		Curl.global_cleanup ()

	let use = Lwt_pool.use
end

let fetch (ctx: Ctx.t) ~dest url : (unit, [> error] ) Result.t Lwt.t =
	Ctx.use ctx (fun () ->
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
	)
