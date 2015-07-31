module JSON = Yojson.Safe
module OPAM = OpamFile.OPAM

type env = {
	opam_vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	spec : OpamFile.OPAM.t;
	files : string option;
}

let os () =
	let os =
		let open Lwt in
		lwt lines = Lwt_process.with_process_in ("", [|"uname"; "-s"|]) (fun proc ->
			lwt lines = proc#stdout |> Lwt_io.read_lines |> Lwt_stream.to_list in
			lwt status = proc#close in
			let open Unix in
			match status with
				| WEXITED 0 -> return lines
				| _ -> failwith "uname -s failed"
		) in
		return (match lines with
			| [line] -> String.lowercase line
			| _ -> failwith ("Unexpected uname output:\n" ^ (String.concat "\n" lines))
		)
	in
	Lwt_main.run os


let unexpected_json desc j =
	failwith ("Unexpected " ^ desc ^ ": " ^ (JSON.pretty_to_string ~std:true j))

let load_env () =
	let json_str = Unix.getenv "opamEnv" in
	let json = JSON.from_string json_str in
	let open OpamTypes in
	let state = ref OpamVariable.Full.Map.empty in
	let add_var name v = state := !state |> OpamVariable.Full.Map.add (OpamVariable.Full.of_string name) v in
	add_var "os" (S (os ()));
	add_var "make" (S "make");
	let destDir = (Unix.getenv "out") in
	add_var "prefix" (S destDir);

	let dir name = S (Filename.concat destDir name) in
	add_var "bin" (dir "bin");
	add_var "lib" (dir "lib");
	add_var "man" (dir "man");
	add_var "preinstalled" (B true); (* well, it's not installed by OPAM ... *)


	let spec = ref None in
	let files = ref None in
	(* let specfile = ref None in *)
	let () = match json with
		| `Assoc pairs -> begin
			pairs |> List.iter (function
					| "deps", `Assoc (attrs) -> begin
						attrs |> List.iter (fun (name, value) ->
							let enabled_var = name^":installed" in
							match value with
								| `Null -> add_var enabled_var (B false)
								| `String _ -> add_var enabled_var (B true)
								| other -> unexpected_json "`deps`" other
						)
					end
					| "files", `String path -> files := Some path
					| "spec", `String path -> spec := Some (Opam_metadata.load_opam path)
					| other, _ -> failwith ("unexpected opamEnv key: " ^ other)
			)
		end
		| other -> unexpected_json "toplevel" other
	in
	{
		opam_vars = !state;
		spec = (match !spec with Some s -> s | None -> failwith "missing `spec` in opamEnv");
		files = !files;
	}

let lookup env key =
	try Some (OpamVariable.Full.Map.find key env.opam_vars)
	with Not_found -> begin
		prerr_endline ("WARN: opam var " ^ (OpamVariable.Full.to_string key) ^ " not found...");
		None
	end

let run env get_commands =
	let commands = get_commands env.spec in
	(* let () = commands |> OpamFilter.commands_variables |> List.map OpamVariable.Full.to_string |> String.concat " " |> Printf.eprintf "XXX vars: %s\n" in *)
	let commands = commands |> OpamFilter.commands (lookup env) in
	commands |> List.iter (fun args ->
		match args with
			| [] -> ()
			| command :: _ -> begin
				let open Unix in
				prerr_endline ("+ " ^ String.concat " " args);
				let pid = create_process command (args |> Array.of_list) stdin stdout stderr in
				let (_, status) = waitpid [] pid in
				let quit code = prerr_endline "Command failed."; exit code in
				match status with
					| WEXITED 0 -> ()
					| WEXITED code -> quit code
					| _ -> quit 1
			end
	)

let build env = run env OPAM.build
let install env = run env OPAM.install

let () =
	let action = match Sys.argv with
		| [| _; "build" |] -> build
		| [| _; "install" |] -> install
		| _ -> failwith "UsageError"
	in
	action (load_env ())

