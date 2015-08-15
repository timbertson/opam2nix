
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
	let opts = Arg.align [
		("--src", Arg.Set_string repo, "Opam repository");
		("--dest", Arg.Set_string dest, "Destination (must not exist, unless --unclean given)");
		("--cache", Arg.Set_string cache, "Cache (may exist)");
		("--unclean", Arg.Set merge_existing, "Write into an existing destination");
		("--max-age", Arg.Set_int max_age, "Maximum cache age (hours)");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse_argv ~current:(ref arg_idx) args opts add_package "TODO: usage...";

	let packages = List.rev !packages in
	let repo = nonempty !repo "--repo" in
	let dest = nonempty !dest "--dest" in
	let cache = nonempty !cache "--cache" in

	let mkdir dest = Unix.mkdir dest 0o750 in

	(* first, make `dest` (don't care if it exists) *)
	let () =
		try mkdir dest
		with Unix.Unix_error(Unix.EEXIST, _, _) -> ()
	in

	(* then make `dest/packages` (only use existing if --unclean specified) *)
	let dest = Filename.concat dest "packages" in
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

	let () = try
		mkdir cache
	with Unix.Unix_error(Unix.EEXIST, _, _) -> () in

	let cache = new File_cache.cache ~max_age:(!max_age*60*60) cache in

	let deps = new Opam_metadata.dependency_map in

	let write_expr path expr =
		Printf.eprintf "Generating %s ...\n" path;
		flush stderr;
		let expr = expr () in
		let oc = open_out path in
		Nix_expr.write oc expr;
		close_out oc
	in

	Repo.traverse `Opam ~repo ~packages (fun package version path ->
		let dest_parts = [package; version] in
		mkdirp_in dest dest_parts;
		let version_dir = String.concat Filename.dir_sep (dest :: dest_parts) in
		let dest_path = Filename.concat version_dir "default.nix" in
		let files_src = (Filename.concat path "files") in
		let has_files = try let (_:Unix.stats) = Unix.stat files_src in true with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
		let open FileUtil in
		cp [Filename.concat path "opam"] (Filename.concat version_dir "opam");
		let () =
			let filenames = if Sys.file_exists files_src then ls files_src else [] in
			match filenames with
				| [] -> ()
				| filenames ->
					(* copy all the files *)
					let files_dest = Filename.concat version_dir "files" in
					mkdirp_in version_dir ["files"];
					cp filenames files_dest;
		in

		write_expr dest_path (fun () ->
			Opam_metadata.nix_of_opam ~cache ~deps ~has_files ~name:package ~version path
		)
	);

	Repo.traverse_versions ~root:dest (fun package impls base ->
		let path_of_version = (fun ver -> `Lit ("import ./" ^ ver)) in
		let path = Filename.concat base "default.nix" in
		write_expr path (fun () ->
			`Attrs (Nix_expr.AttrSet.build (
				("latest", impls |> Repo.latest_version |> path_of_version) ::
				(impls |> List.map (fun ver -> ver, path_of_version ver))
			))
		)
	);

	let () =
		let packages = list_dirs dest in
		let path_of_package = (fun p -> `Lit ("import ./" ^ p)) in
		let path = Filename.concat dest "default.nix" in
		write_expr path (fun () ->
			`Attrs (Nix_expr.AttrSet.build (
				packages |> List.map (fun ver -> ver, path_of_package ver)
			))
		)
	in

	(* Printf.eprintf "generated deps: %s\n" (deps#to_string) *)
	()
