
open Printf
open Util

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

let main arg_idx args =
	let repo = ref "" in
	let dest = ref "" in
	let cache = ref "" in
	let max_age = ref (14*24) in
	let merge_existing = ref false in
	let num_versions = ref None in
	let package_selection = ref [] in
	let ignore_broken_packages = ref false in
	let opts = Arg.align [
		("--src", Arg.Set_string repo, "DIR Opam repository");
		("--dest", Arg.Set_string dest, "DIR Destination (must not exist, unless --unclean given)");
		("--num-versions", Arg.Int (fun n -> num_versions := Some n), "NUM Versions of each *-versioned package to keep (default: all)");
		("--cache", Arg.Set_string cache, "DIR Cache (may exist)");
		("--unclean", Arg.Set merge_existing, "(bool) Write into an existing destination");
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

	let repo = nonempty !repo "--repo" in
	let dest = nonempty !dest "--dest" in
	let cache = match !cache with
		| "" -> Filename.concat (XDGBaseDir.Cache.user_dir ()) "opam2nix/download"
		| other -> other
	in

	let mkdir dest = Unix.mkdir dest 0o750 in

	(* then make `dest/packages` (only use existing if --unclean specified) *)
	let () = try
		mkdir dest
	with Unix.Unix_error(Unix.EEXIST, _, _) -> (
		if !merge_existing then
			Printf.eprintf "Adding to existing contents at %s\n" dest
		else begin
			Unix.rmdir dest;
			mkdir dest
		end
	) in

	FileUtil.mkdir ~parent:true cache;

	let cache = new File_cache.cache ~max_age:(!max_age*60*60) cache in

	let deps = new Opam_metadata.dependency_map in

	let generate_expr ~path expr =
		Printf.eprintf "Generating %s ...\n" path;
		flush stderr;
		expr ()
	in

	let write_expr path expr =
		let oc = open_out path in
		Nix_expr.write oc expr;
		close_out oc
	in

	(* if `--num-versions is specified, swap the `All entries to
	 * a n-latest filter *)
	let package_selection : Repo.package_selection list = match !num_versions with
		| Some n ->
			let filter = `Filter (Repo.version_filter n) in
			package_selection |> List.map (function
				| `All -> `Filtered filter
				| `Package (name, `All) -> `Package (name, filter)
				| other -> other
			)
		| None -> package_selection
	in

	Repo.traverse `Opam ~repos:[repo] ~packages:package_selection (fun package version path ->
		let dest_parts = [package; version] in
		let version_dir = String.concat Filename.dir_sep (dest :: dest_parts) in
		let dest_path = Filename.concat version_dir "default.nix" in
		let files_src = (Filename.concat path "files") in
		let has_files = try let (_:Unix.stats) = Unix.stat files_src in true with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
		let expr = generate_expr ~path:dest_path (fun () ->
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
						mkdirp_in version_dir ["files"];
						cp ~recurse:true ~preserve:true ~force:Force filenames files_dest;
			in
			write_expr dest_path expr
		)

	);

	Repo.traverse_versions ~root:dest (fun package impls base ->
		let path_of_version = (fun ver -> `Lit ("import ./" ^ ver ^ " world")) in
		let path = Filename.concat base "default.nix" in
		write_expr path (
			`Function (`Id "world",
				`Attrs (Nix_expr.AttrSet.build (
					("latest", impls |> Repo.latest_version |> path_of_version) ::
					(impls |> List.map (fun ver -> ver, path_of_version ver))
				))
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
