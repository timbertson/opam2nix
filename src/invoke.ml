module JSON = Yojson.Safe
module OPAM = OpamFile.OPAM

type env = {
	opam_vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	spec : OpamFile.OPAM.t;
	files : string option;
	pkgname : string;
}

let destDir () = (Unix.getenv "out")
let ocamlfindDestDir () =
	try Some (Unix.getenv "OCAMLFIND_DESTDIR")
	with Not_found -> None
let libDestDir () = Filename.concat (destDir ()) "lib"

let unexpected_json desc j =
	failwith ("Unexpected " ^ desc ^ ": " ^ (JSON.pretty_to_string j))

let load_env () =
	let json_str = Unix.getenv "opamEnv" in
	let json = JSON.from_string json_str in
	let open OpamTypes in
	let pkgname = ref "" in
	let state = ref (Opam_metadata.init_variables ()) in
	let add_var name v = state := !state |> Opam_metadata.add_var name v in
	let destDir = destDir () in
	add_var "prefix" (S destDir);

	let dir name = S (Filename.concat destDir name) in
	add_var "bin" (dir "bin");
	add_var "lib" (dir "lib");
	add_var "man" (dir "man");
	add_var "doc" (dir "share/doc");


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

								(* Bool is only used for base packages,
								 * (although it's only ever `true`) *)
								| `Bool b -> add_var enabled_var (B b)
								| other -> unexpected_json "`deps`" other
						)
					end
					| "deps", other -> unexpected_json "deps" other

					| "name", `String name -> pkgname := name
					| "name", other -> unexpected_json "name" other

					| "files", `String path -> files := Some path
					| "files", `Null -> ()
					| "files", other -> unexpected_json "files" other

					| "spec", `String path -> spec := Some (Opam_metadata.load_opam path)
					| "spec", other -> unexpected_json "spec" other

					| "ocaml-version", `String version -> add_var "ocaml-version" (S version)
					| "ocaml-version", other -> unexpected_json "ocaml-version" other

					| other, _ -> failwith ("unexpected opamEnv key: " ^ other)
			)
		end
		| other -> unexpected_json "toplevel" other
	in
	{
		opam_vars = !state;
		pkgname = !pkgname;
		spec = (match !spec with Some s -> s | None -> failwith "missing `spec` in opamEnv");
		files = !files;
	}

let run env get_commands =
	let commands = get_commands env.spec in
	let commands = commands |> OpamFilter.commands (Opam_metadata.lookup_var env.opam_vars) in
	commands |> List.iter (fun args ->
		match args with
			| [] -> ()
			| command :: _ -> begin
				let open Unix in
				prerr_endline ("+ " ^ String.concat " " args);
				let pid = create_process command (args |> Array.of_list) stdin stdout stderr in
				let rec keep_waiting pid =
					try waitpid [] pid
					with Unix.Unix_error(EINTR, _, _) -> keep_waiting pid
				in
				let (_, status) = keep_waiting pid in
				let quit code = prerr_endline "Command failed."; exit code in
				match status with
					| WEXITED 0 -> ()
					| WEXITED code -> quit code
					| _ -> quit 1
			end
	)

let ensure_dir_exists d =
	if not (OpamFilename.exists_dir d) then (
		Printf.eprintf "creating %s\n" (OpamFilename.Dir.to_string d);
		OpamFilename.mkdir d;
	)

let execute_install_file state =
	let name = state.pkgname in
	let open OpamTypes in
	(* XXX this should really be exported from opam proper... *)
	let warnings = ref [] in
	let check ~src ~dst base =
		let src_file = OpamFilename.create src base.c in
		let exists = OpamFilename.exists src_file in
		if base.optional && not exists then
			Printf.eprintf "Not installing missing optional file: %s\n"
				(OpamFilename.to_string src_file);
		if not base.optional && not exists then
			warnings := (dst, base.c) :: !warnings;
		exists
	in
	let cwd = OpamFilename.cwd () in

	let install_f = OpamFilename.raw (name ^ ".install") in
	let install = OpamFile.Dot_install.safe_read install_f in
	if OpamFilename.exists install_f
		then prerr_endline "Installing from .install file!"
		else prerr_endline "no .install file found!";

	(* Install a list of files *)
	let destDir = destDir () |> OpamFilename.Dir.of_string in
	let libDestDir = libDestDir () |> OpamFilename.Dir.of_string in
	let build_dir = cwd in
	let install_files exec destBase dest files_fn =
		let open OpamFilename.OP in
		let destDir = destBase / dest in
		let files = files_fn install in
		match files with
			| [] -> ()
			| files ->
				ensure_dir_exists destDir;
				List.iter (fun (base, dst) ->
					let src_file = OpamFilename.create build_dir base.c in
					let dst_file = match dst with
						| None   -> OpamFilename.create destDir (OpamFilename.basename src_file)
						| Some d -> OpamFilename.create destDir d in
					if check ~src:build_dir ~dst:destDir base then
						OpamFilename.install ~exec ~src:src_file ~dst:dst_file ();
				) files
	in

	(* bin *)
	install_files true destDir "bin" OpamFile.Dot_install.bin;

	(* sbin *)
	install_files true destDir "sbin" OpamFile.Dot_install.sbin;

	(* lib *)
	install_files false libDestDir name OpamFile.Dot_install.lib;
	install_files true libDestDir name OpamFile.Dot_install.libexec;

	(* toplevel *)
	install_files false libDestDir "toplevel" OpamFile.Dot_install.toplevel;

	install_files true destDir "lib" OpamFile.Dot_install.stublibs;

	(* Man pages *)
	install_files false destDir "man" OpamFile.Dot_install.man;

	(* Shared files *)
	install_files false destDir "share" OpamFile.Dot_install.share;
	install_files false destDir "share" OpamFile.Dot_install.share_root;

	(* Etc files *)
	install_files false destDir "etc" OpamFile.Dot_install.etc;

	(* Documentation files *)
	install_files false destDir "doc" OpamFile.Dot_install.doc;

	(* misc: not allowed. *)

	if !warnings <> [] then (
		let print (dir, base) =
			Printf.sprintf "  - %s to %s\n"
				(OpamFilename.to_string (OpamFilename.create build_dir base))
				(OpamFilename.Dir.to_string dir) in
		OpamGlobals.error "Installation failed!";
		let msg =
			Printf.sprintf
				"Some files in %s couldn't be installed:\n%s"
				(OpamFilename.prettify install_f)
				(String.concat "" (List.map print !warnings))
		in
		failwith msg
	)

let isdir path =
	let open Unix in
	try (stat path).st_kind = S_DIR
	with Unix_error(ENOENT, _, _) -> false

(* NOTE: unused - delete if we don't want to go back to using this *)
let fixup_opam_install env =
	Printf.eprintf "Running post-opam install fixup ...\n";
	let name = env.pkgname in
	let lib_dest = Filename.concat (destDir ()) "lib" in
	let unwanted = Filename.concat lib_dest name in
	let fixed = ocamlfindDestDir () in
	match fixed with
		| None -> Printf.eprintf "$OCAMLFIND_DESTDIR not set, skipping\n"; ()
		| Some ocamlfind_dest -> (
			let ocamlfind_dest = Filename.concat ocamlfind_dest name in
			if isdir unwanted then (
				(* XXX is installing under lib/ocaml/version/pkg instead of just lib/ really worth all this hassle? *)
				if Sys.file_exists ocamlfind_dest then
					Printf.eprintf
						"WARN: Looks like incorrectly-installed ocaml libraries in %s\n - but dest (%s) already exists; ignoring...\n"
						unwanted ocamlfind_dest
				else (
					Printf.eprintf
						"WARN: Found incorrectly-installed ocaml libraries in %s\n - moving them to %s\n"
						unwanted ocamlfind_dest;
					Unix.rename unwanted ocamlfind_dest
				)
			) else (
				Printf.eprintf "No unwanted files found in %s\n" unwanted;
			);

			(* If we have `lib/pkgconfig`, make a symlink `lib/<pkgname>` to `lib/ocaml/<...>/pkgname` *)
			let pkgconfig = Filename.concat lib_dest "pkgconfig" in
			if isdir pkgconfig then (
				if not (Sys.file_exists unwanted) then (
					Printf.eprintf "NOTE: linking %s -> %s for the benefit of pkgconfig\n" unwanted ocamlfind_dest;
					Unix.symlink ocamlfind_dest unwanted
				) else (
					Printf.eprintf "NOTE: can't create link %s -> %s\n" unwanted ocamlfind_dest;
				)
			)
		)

let build env =
	let destDir = destDir () |> OpamFilename.Dir.of_string in
	ensure_dir_exists destDir;
	run env OPAM.build

let install env =
	run env OPAM.install;
	execute_install_file env

let main idx args =
	let action = try Some (Array.get args (idx+1)) with Not_found -> None in
	let action = match action with
		| Some "build" -> build
		| Some "install"-> install
		| Some other -> failwith ("Unknown action: " ^ other)
		| None -> failwith "No action given"
	in
	Unix.putenv "PREFIX" (destDir ());
	action (load_env ())

