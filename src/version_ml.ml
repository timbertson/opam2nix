let () =
	let version = 
		let f = open_in "../VERSION" in
		let v = input_line f in
		close_in f;
		v
	in

	let git_rev = try
		let stdout = Unix.open_process_in "git rev-parse HEAD" in
		let rev = input_line stdout in
		close_in stdout;
		Some rev
	with
		| Unix.Unix_error _ -> None
		| End_of_file -> None
	in

	let version = match git_rev with
		| Some v -> version ^ ":" ^ v
		| None -> version
	in
	let out = open_out Sys.argv.(1) in
	[
		"let main _idx _args =";
		"	print_endline \"opam2nix: " ^ version ^ "\";";
		"	print_endline (\"opam: \" ^ OpamVersion.(to_string current));";
	] |> List.iter (fun line ->
		output_string out line;
		output_char out '\n'
	);
	close_out out
