module JSON = Yojson.Basic
module OPAM = OpamFile.OPAM
open Util

type env = {
	opam_vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	spec : OpamFile.OPAM.t;
	files : string option;
	pkgname : string;
}

type package_implementation =
	| Provided
	| Installed of string
	| Absent

type package_relation =
	| Dependency of string
	| Self of string

let destDir () = (Unix.getenv "out")
let libDestDir () = Filename.concat (destDir ()) "lib"

let unexpected_json desc j =
	failwith ("Unexpected " ^ desc ^ ": " ^ (JSON.pretty_to_string j))

let load_env () =
	let open OpamTypes in
	let pkgname = ref "" in
	let state = ref (Opam_metadata.init_variables ()) in
	let destDir = destDir () in
	let add_global_var name value =
		state := !state |> Opam_metadata.add_global_var name value
	in
	let add_package_var pkg name value =
		state := !state |> Opam_metadata.add_package_var pkg name value
	in

	let add_package_vars ~pkg impl =
		let add_var = match pkg with
			| Dependency pkgname ->
					add_package_var pkgname
			| Self pkgname -> fun name value ->
					add_package_var pkgname name value;
					add_global_var name value
		in
		(match pkg with
			| Self name -> add_var "name" (S name)
			| _ -> ()
		);
		(match impl with
			| Absent ->
				add_var "installed" (B false);
			| Provided ->
				add_var "installed" (B true);
			| Installed path ->
				add_var "installed" (B true);

				let dir suffix = S (Filename.concat path suffix) in
				let package_dir suffix =
					let base = (Filename.concat path suffix) in
					match pkg with
						| Dependency pkg -> S (Filename.concat base pkg)
						(* XXX this difference for the current package seems to be how opam works.
						 * It doesn't seem to be documented anywhere, but odoc's build relies on it *)
						| Self _ -> S base
				in

				(* https://opam.ocaml.org/doc/Manual.html#package-name-install *)
				add_var "bin" (dir "bin");
				add_var "stublibs" (dir "lib/stublibs");
				add_var "man" (dir "man");

				add_var "lib" (package_dir "lib");
				add_var "libexec" (package_dir "libexec");
				add_var "etc" (package_dir "etc");
				add_var "doc" (package_dir "doc");
				add_var "share" (package_dir "share");
		)
	in

	add_global_var "prefix" (S destDir);

	let spec = ref None in
	let files = ref None in
	(* let specfile = ref None in *)
	let json_str = Unix.getenv "opamEnv" in
	debug "Using opamEnv: %s\n" json_str;
	let json = JSON.from_string json_str in
	let () = match json with
		| `Assoc pairs -> begin
			pairs |> List.iter (function
					| "deps", `Assoc (attrs) -> begin
						debug "adding deps from opamEnv\n";
						attrs |> List.iter (fun (pkgname, value) ->
							let pkg = Dependency pkgname in
							match value with
								| `Null ->
									debug " - package %s is absent (null)\n" pkgname;
									add_package_vars ~pkg Absent

								| `Bool b ->
									(* Bool is used for base packages, which have no corresponding path *)
									debug " - base package %s: present? %b\n" pkgname b;
									add_package_vars ~pkg (if b then Provided else Absent)

								| `String path ->
									debug " - package %s: installed at %s\n" pkgname path;
									add_package_vars ~pkg (Installed path);

								| other -> unexpected_json "`deps`" other
						)
					end
					| "deps", other -> unexpected_json "deps" other

					| "name", `String name ->
							pkgname := name;
							add_package_vars ~pkg:(Self name) (Installed destDir)
					| "name", other -> unexpected_json "name" other

					| "files", `String path -> files := Some path
					| "files", `Null -> ()
					| "files", other -> unexpected_json "files" other

					| "spec", `String path ->
							Printf.eprintf "Loading %s\n" path;
							spec := Some (Opam_metadata.load_opam path)
					| "spec", other -> unexpected_json "spec" other

					| "ocaml-version", `String version ->
							add_package_var "ocaml" "version" (S version)
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

let rec waitpid_with_retry flags pid =
	let open Unix in
	try waitpid flags pid
	with Unix_error (EINTR, _, _) -> waitpid_with_retry flags pid

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
				let (_, status) = waitpid_with_retry [] pid in
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

let remove_empty_dir d =
	if OpamFilename.dir_is_empty d then (
		Printf.eprintf "removing empty dir %s\n" (OpamFilename.Dir.to_string d);
		OpamFilename.rmdir d;
	)

let execute_install_file state =
	let name = state.pkgname in
	let install_file_path = (name ^ ".install") in
	if (Sys.file_exists install_file_path) then (
		prerr_endline ("Installing from " ^ install_file_path);
		let cmd =  [|
			"opam-installer"; "--prefix"; destDir (); install_file_path
		|] in
		let cmd_desc = String.concat " " (Array.to_list cmd) in
		prerr_endline (" + " ^ cmd_desc);
		let open Unix in
		let pid = Unix.create_process (Array.get cmd 0) cmd Unix.stdin Unix.stdout Unix.stderr in
		match waitpid_with_retry [ WUNTRACED ] pid with
			| (_, WEXITED 0) -> ()
			| _ -> failwith (cmd_desc ^ " failed")
	) else (
		prerr_endline "no .install file found!";
	)

let isdir path =
	let open Unix in
	try (stat path).st_kind = S_DIR
	with Unix_error(ENOENT, _, _) -> false

(* NOTE: unused - delete if we don't want to go back to using this *)
let ocamlfindDestDir () =
	try Some (Unix.getenv "OCAMLFIND_DESTDIR")
	with Not_found -> None
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

let apply_patches env =
	(* extracted from OpamAction.prepare_package_build *)
	let opam = env.spec in
	let filter_env = Opam_metadata.lookup_var env.opam_vars in

	(* Substitute the patched files.*)
	let patches = OpamFile.OPAM.patches opam in

	let iter_patches f =
		List.fold_left (fun acc (base, filter) ->
			let fail e =
				OpamStd.Exn.fatal e;
				OpamFilename.Base.to_string base :: acc
			in
			if OpamFilter.opt_eval_to_bool (filter_env) filter
			then
				let result = try f base with e -> Some e in
				match result with
					| Some (e: exn) -> fail e
					| None -> acc
			else acc
		) [] patches in

	let all = OpamFile.OPAM.substs opam in
	let patches =
		OpamStd.List.filter_map (fun (f,_) ->
			if List.mem f all then Some f else None
		) patches in
	List.iter
		(OpamFilter.expand_interpolations_in_file (filter_env))
		patches;

	(* Apply the patches *)
	let patching_errors =
		iter_patches (fun filename ->
			let filename_str = (OpamFilename.Base.to_string filename) in
			Printf.eprintf "applying patch: %s\n" filename_str;
			OpamSystem.patch ~dir:(Sys.getcwd ()) filename_str |> OpamProcess.Job.run
		)
	in

	(* Substitute the configuration files. We should be in the right
		 directory to get the correct absolute path for the
		 substitution files (see [substitute_file] and
		 [OpamFilename.of_basename]. *)
	List.iter
		(OpamFilter.expand_interpolations_in_file (filter_env))
		(OpamFile.OPAM.substs opam);

	if patching_errors <> [] then (
		let msg =
			Printf.sprintf "These patches didn't apply:\n%s"
				(OpamStd.Format.itemize (fun x -> x) patching_errors)
		in
		failwith msg
	)

let binDir dest =
	let open OpamFilename.Op in
	dest / "bin"

let libDir dest =
	let open OpamFilename.Op in
	dest / "lib"

let outputDirs dest = [ binDir dest; libDir dest ]

let pre_build env =
	let dest = destDir () |> OpamFilename.Dir.of_string in
	ensure_dir_exists dest;
	outputDirs dest |> List.iter ensure_dir_exists;
	apply_patches env

let build env =
	pre_build env;
	run env OPAM.build

let install env =
	run env OPAM.install;
	execute_install_file env;
	let dest = destDir () |> OpamFilename.Dir.of_string in
	outputDirs dest |> List.iter remove_empty_dir

let main idx args =
	let action = try Some (Array.get args (idx+1)) with Not_found -> None in
	let action = match action with
		| Some "prebuild" -> pre_build
		| Some "build" -> build
		| Some "install"-> install
		| Some other -> failwith ("Unknown action: " ^ other)
		| None -> failwith "No action given"
	in
	Unix.putenv "PREFIX" (destDir ());
	action (load_env ())

