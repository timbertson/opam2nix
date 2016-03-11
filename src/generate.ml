
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

type package_selection =
	[ `Latest_versions of int
	| `Package_list of string list
	]

let main arg_idx args =
	let repo = ref "" in
	let dest = ref "" in
	let cache = ref "" in
	let max_age = ref (14*24) in
	let merge_existing = ref false in
	let package_selection = ref None in
	let opts = Arg.align [
		("--src", Arg.Set_string repo, "DIR Opam repository");
		("--dest", Arg.Set_string dest, "DIR Destination (must not exist, unless --unclean given)");
		("--keep-versions", Arg.Int (fun n -> package_selection := Some (`Latest_versions n)), "NUM Versions of each package to keep (default 0)");
		("--cache", Arg.Set_string cache, "DIR Cache (may exist)");
		("--unclean", Arg.Set merge_existing, "(bool) Write into an existing destination");
		("--max-age", Arg.Set_int max_age, "HOURS Maximum cache age");
	]; in
	let add_package p =
		let existing = match !package_selection with
			| Some (`Package_list packages) -> packages
			| None -> []
			| Some (`Latest_versions _) ->
				failwith "Can't specify --per-package and individual package specs"
		in
		package_selection := Some (`Package_list (p :: existing))
	in
	Arg.parse_argv ~current:(ref arg_idx) args opts add_package "usage: opam2nix generate [OPTIONS] [package@version [package2@version2]]";
	
	(* fix up reversed package list *)
	let package_selection = match !package_selection with
		| Some (`Package_list p) -> (`Package_list (List.rev p))
		| Some (`Latest_versions _ as sel) -> sel
		| None ->
			prerr_endline "no packages selected";
			exit 0
	in

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

	let write_expr path expr =
		Printf.eprintf "Generating %s ...\n" path;
		flush stderr;
		let expr = expr () in
		let oc = open_out path in
		Nix_expr.write oc expr;
		close_out oc
	in


	let package_selection = match package_selection with
		| `Package_list p -> `List p
		| `Latest_versions n -> `Filter (fun _package versions ->
			let dot = Str.regexp "\\." in
			let keep = ref [] in
			Repo.decreasing_version_order versions |> List.iter (fun version ->
				let major_minor v =
					let parts = Str.split dot v |> List.rev in
					match parts with
						| [] -> []
						| patch::parts -> List.rev parts
				in
				let base_version = major_minor version in
				try
					let predicate = fun candidate -> major_minor candidate = base_version in
					let _:string = List.find predicate !keep in ()
				with Not_found -> begin
					keep := version :: !keep
				end
			);
			!keep |> take 3 
		)
	in
	Repo.traverse `Opam ~repos:[repo] ~packages:package_selection (fun package version path ->
		let dest_parts = [package; version] in
		mkdirp_in dest dest_parts;
		let version_dir = String.concat Filename.dir_sep (dest :: dest_parts) in
		let dest_path = Filename.concat version_dir "default.nix" in
		let files_src = (Filename.concat path "files") in
		let has_files = try let (_:Unix.stats) = Unix.stat files_src in true with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
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
					cp filenames files_dest;
		in

		write_expr dest_path (fun () ->
			Opam_metadata.nix_of_opam ~cache ~deps ~has_files ~name:package ~version path
		)
	);

	Repo.traverse_versions ~root:dest (fun package impls base ->
		let path_of_version = (fun ver -> `Lit ("import ./" ^ ver ^ " world")) in
		let path = Filename.concat base "default.nix" in
		write_expr path (fun () ->
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
		write_expr path (fun () ->
			`Function (`Id "world",
				`Attrs (Nix_expr.AttrSet.build (
					packages |> List.map (fun ver -> ver, path_of_package ver)
				))
			)
		)
	in

	(* Printf.eprintf "generated deps: %s\n" (deps#to_string) *)
	()
