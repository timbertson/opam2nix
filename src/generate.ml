
open Printf
open Util
module StringMap = struct
	include Map.Make(String)
	let find_opt key map = try Some (find key map) with Not_found -> None
end

let rec mkdirp_in base dirs =
	let relpath = String.concat Filename.dir_sep dirs in
	let fullpath = Filename.concat base relpath in
	let fail () = failwith ("Not a directory: " ^ fullpath) in
	let open Unix in
	try
		if (stat fullpath).st_kind != S_DIR then fail ()
	with Unix_error (ENOENT, _, _) -> begin
		let () = match (List.rev dirs) with
			| [] -> fail ()
			| dir :: prefix -> mkdirp_in base (List.rev prefix)
		in
		mkdir fullpath 0o0750
	end

type update_mode = [
	| `clean
	| `unclean
	| `update
]

type 'a generated_expression = [ | `reuse_existing | `expr of 'a ]

let main arg_idx args =
	let repo = ref "" in
	let dest = ref "" in
	let cache = ref "" in
	let max_age = ref (14*24) in
	let update_mode = ref `clean in
	let num_versions = ref None in
	let package_selection = ref [] in
	let ignore_broken_packages = ref false in
	let opts = Arg.align [
		("--src", Arg.Set_string repo, "DIR Opam repository");
		("--dest", Arg.Set_string dest, "DIR Destination (must not exist, unless --unclean / --update given)");
		("--num-versions", Arg.String (fun n -> num_versions := Some n), "NUM Versions of each *-versioned package to keep (default: all. Format: x.x.x)");
		("--cache", Arg.Set_string cache, "DIR Cache (may exist)");

		("--unclean", Arg.Unit (fun () ->
			update_mode := match !update_mode with
				| `clean -> `unclean
				| `unclean | `update as m -> m
			), "(bool) Write into an existing destination (no cleanup, leaves existing files)"
		);

		("--update", Arg.Unit (fun () ->
				update_mode := `update
			), "(bool) Update an existing destination. Old versions will be removed and new ones added. files and opam contents will be updated, but .nix files won't be regenerated"
		);

		("--max-age", Arg.Set_int max_age, "HOURS Maximum cache age");
		("--ignore-broken", Arg.Set ignore_broken_packages, "(bool) skip over unprocessible packages (default: fail)");
	]; in
	let add_package p = package_selection := (match p with
			| "*" -> `All
			| p -> `Package (Repo.parse_package_spec p)
		) :: !package_selection
	in
	Arg.parse_argv ~current:(ref arg_idx) args opts add_package "usage: opam2nix generate [OPTIONS] [package@version [package2@version2]]";
	
	(* fix up reversed package list *)
	let package_selection = List.rev !package_selection in
	let () = if List.length package_selection == 0 then (
		prerr_endline "no packages selected (did you mean '*'?)";
		exit 0
	) in

	let repo = nonempty !repo "--src" in
	let dest = nonempty !dest "--dest" in
	let cache = match !cache with
		| "" -> Filename.concat (XDGBaseDir.Cache.user_dir ()) "opam2nix/download"
		| other -> other
	in
	let mode = !update_mode in

	let mkdir dest = Unix.mkdir dest 0o750 in

	(* then make `dest/packages` (only use existing if --unclean specified) *)
	let () = try
		mkdir dest
	with Unix.Unix_error(Unix.EEXIST, _, _) -> (
		match mode with
			| `clean ->
				Unix.rmdir dest;
				mkdir dest
			| `unclean ->
				Printf.eprintf "Adding to existing contents at %s\n" dest
			| `update ->
				Printf.eprintf "Updating existing contents at %s\n" dest
	) in

	FileUtil.mkdir ~parent:true cache;

	let cache = new File_cache.cache ~max_age:(!max_age*60*60) cache in

	let deps = new Opam_metadata.dependency_map in

	let generate_expr ~mode ~path expr =
		if mode = `update && Sys.file_exists path then (
			(* Printf.eprintf "Using existing %s ...\n" path; *)
			Some `reuse_existing
		) else (
			Printf.eprintf "Generating %s ...\n" path;
			flush stderr;
			expr () |> Option.map (fun expr -> `expr expr)
		)
	in

	let write_expr path expr =
		let oc = open_out path in
		Nix_expr.write oc expr;
		close_out oc
	in

	(* if `--num-versions is specified, swap the `All entries for a filter *)
	let package_selection : Repo.package_selection list = match !num_versions with
		| Some n ->
			let filter = Repo.parse_version_filter n in
			package_selection |> List.map (function
				| `All -> `Filtered filter
				| `Package (name, `All) -> `Package (name, filter)
				| other -> other
			)
		| None -> package_selection
	in

	let generated_versions = ref StringMap.empty in
	let mark_version_generated ~package version =
		let current = !generated_versions in
		generated_versions := match StringMap.find_opt package current with
			| Some versions ->
				StringMap.add package (version :: versions) current
			| None ->
				StringMap.add package [version] current
	in

	Repo.traverse `Opam ~repos:[repo] ~packages:package_selection (fun package version path ->
		let dest_parts = [package; (Repo.path_of_version `Nix version)] in
		let version_dir = String.concat Filename.dir_sep (dest :: dest_parts) in
		let dest_path = Filename.concat version_dir "default.nix" in
		let files_src = (Filename.concat path "files") in
		let has_files = try let (_:Unix.stats) = Unix.stat files_src in true with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
		let expr = generate_expr ~mode ~path:dest_path (fun () ->
			let handle_error desc e =
				if !ignore_broken_packages then (
					prerr_endline ("Warn: " ^ desc); None
				) else raise e
			in
			let open Opam_metadata in
			try
				Some (nix_of_opam ~cache ~deps ~has_files ~name:package ~version path)
			with
			| Unsupported_archive desc as e -> handle_error ("Unsupported archive: " ^ desc) e
			| Invalid_package desc as e -> handle_error ("Invalid package: " ^ desc) e
			| File_cache.Download_failed url as e -> handle_error ("Download failed: " ^ url) e
		) in
		expr |> Option.may (fun expr ->
			mkdirp_in dest dest_parts;
			let open FileUtil in
			cp [readlink (Filename.concat path "opam")] (Filename.concat version_dir "opam");
			let () =
				let filenames = if Sys.file_exists files_src then ls files_src else [] in
				match filenames with
					| [] -> ()
					| filenames ->
						(* copy all the files *)
						let files_dest = Filename.concat version_dir "files" in
						rm_r files_dest;
						mkdirp_in version_dir ["files"];
						cp ~recurse:true ~preserve:true ~force:Force filenames files_dest;
			in
			mark_version_generated ~package version;
			match expr with
				| `reuse_existing -> ()
				| `expr expr -> write_expr dest_path expr
		)
	);

	Repo.traverse_versions `Nix ~root:dest (fun package versions base ->
		let versions = match mode with
			| `clean | `unclean -> versions
			| `update ->
				(* clean up versions which are just lingering from previous runs *)
				let generated_versions =
					try StringMap.find package !generated_versions
					with Not_found -> []
				in
				(* Printf.eprintf "found versions: [%s] for package %s\n" (String.concat " " generated_versions) package; *)
				let wanted, unwanted = List.partition (fun v -> List.mem v generated_versions) versions in
				unwanted |> List.iter (fun v ->
					let path = Filename.concat base (Repo.path_of_version `Nix v) in
					Printf.eprintf "Removing previous version: %s\n" path;
					rm_r path
				);
				wanted
		in
		if versions = [] then (
			(* only happens with `--update` *)
			Printf.eprintf "Removing package dir which has no versions: %s\n" base;
			rm_r base
		) else (

			let import_version ver =
				(* If the version has special characters, quote it.
				* e.g `import ./fpo`, vs `import (./. + "/foo bar")`
				*)
				let path = Repo.path_of_version `Nix ver in
				`Lit ("import ./" ^ path ^ " world")
			in
			let path = Filename.concat base "default.nix" in
			write_expr path (
				`Function (`Id "world",
					`Attrs (Nix_expr.AttrSet.build (
						("latest", versions |> Repo.latest_version |> import_version) ::
						(versions |> List.map (fun ver -> (Repo.string_of_version ver), import_version ver))
					))
				)
			)
		)
	);

	let () =
		let packages = list_dirs dest in
		let path_of_package = (fun p -> `Lit ("import ./" ^ p ^ " world")) in
		let path = Filename.concat dest "default.nix" in
		write_expr path (
			`Function (`Id "world",
				`Attrs (Nix_expr.AttrSet.build (
					packages |> List.map (fun ver -> ver, path_of_package ver)
				))
			)
		)
	in

	(* Printf.eprintf "generated deps: %s\n" (deps#to_string) *)
	()
