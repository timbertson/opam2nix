open Printf


let without_leading prefix s =
	let prefix_len = String.length prefix in
	let len = String.length s in
	if len >= prefix_len && String.sub s 0 prefix_len = prefix
	then Some (String.sub s prefix_len (len - prefix_len))
	else None

let without_trailing suffix s =
	let suffix_len = String.length suffix in
	let len = String.length s in
	if len >= suffix_len && String.sub s (len - suffix_len) (suffix_len) = suffix
	then Some (String.sub s 0 (len - suffix_len))
	else None


let decreasing_version_order versions =
	let compare a b =
		(* Note: we invert this because we want a descending list *)
		OpamVersion.compare (OpamVersion.of_string b) (OpamVersion.of_string a)
	in
	versions |> List.sort compare

let latest_version versions = List.hd (decreasing_version_order versions)

let rec filter_map fn lst =
	lst |> List.fold_left (fun acc item ->
		match fn item with
			| None -> acc
			| Some result -> result :: acc
	) [] |> List.rev

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

let traverse_repo ~repo ~packages emit =
	let sep = Str.regexp "@" in
	let version_sep = "." in
	let pkgroot = Filename.concat repo "packages" in

	let packages = match packages with
		| [] -> ["*"]
		| packages -> packages
	in

	let process_package package version =
		let package_base = Filename.concat pkgroot package in
		let list_versions () =
			Printf.eprintf "listing %s" package_base;
			let prefix = package ^ version_sep in
			Sys.readdir package_base |> Array.to_list |> filter_map (fun ent ->
				match ent |> without_leading prefix with
					| None -> Printf.eprintf "Skipping non-package directory %s ()\n" ent; None
					| x -> x
			) in

		let versions = match version with
			| None | Some "latest" -> [list_versions () |> latest_version]
			| Some "*" -> list_versions ()
			| Some version -> [version]
		in
		versions |> List.iter (fun version ->
			let path = Filename.concat package_base (package ^ version_sep ^ version) in
			emit package version path
		)
	in

	let list_packages () = Sys.readdir pkgroot |> Array.to_list in
	packages |> List.iter (fun spec ->
		let pkg, version = match Str.split sep spec with
			| [package] -> (package, None)
			| [package; version] -> (package, Some version)
			| _ -> failwith "Invalid package specifier"
		in
		match pkg with
			| "*" -> list_packages () |> List.iter (fun pkg -> process_package pkg version)
			| pkg -> process_package pkg version
	)

let list_dirs root =
	Sys.readdir root |> Array.to_list |> List.filter (fun name ->
		Sys.is_directory (Filename.concat root name)
	)


let traverse_nix_files ~root emit =
	let dirs = list_dirs root in
	dirs |> List.iter (fun pkg ->
		let pkg_path = Filename.concat root pkg in
		let nix_files = Sys.readdir pkg_path
			|> Array.to_list
			|> filter_map (fun name ->
				match name |> without_trailing ".nix" with
					| Some "default" -> None
					| other -> other
		) in
		match nix_files with
			| [] -> ()
			| files -> emit pkg (decreasing_version_order nix_files) pkg_path
	)

let () =
	let repo = ref "" in
	let dest = ref "" in
	let cache = ref "" in
	let merge_existing = ref false in
	let opts = Arg.align [
		("--repo", Arg.Set_string repo, "Repository root");
		("--dest", Arg.Set_string dest, "Destination (must not exist)");
		("--cache", Arg.Set_string cache, "Cache (may exist)");
		("--unclean", Arg.Set merge_existing, "Write into an existing destination");
		(* TODO: --max-age for cache *)
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse opts add_package "TODO: usage...";

	let packages = List.rev !packages in
	let nonempty value arg =
		let rv = !value in
		if rv = "" then failwith (arg ^ " required") else rv in
	let repo = nonempty repo "--repo" in
	let dest = nonempty dest "--dest" in
	let cache = nonempty cache "--cache" in

	let mkdir dest = Unix.mkdir dest 0o750 in
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

	let cache = new File_cache.cache ~max_age:100000 cache in

	let deps = new Opam_metadata.dependency_map in

	let write_expr path expr =
		Printf.eprintf "Generating %s ...\n" path;
		flush stderr;
		let expr = expr () in
		let oc = open_out path in
		Nix_expr.write oc expr;
		close_out oc
	in

	traverse_repo ~repo ~packages (fun package version path ->
		mkdirp_in dest [package];
		let dest_path = String.concat Filename.dir_sep [dest; package; version ^ ".nix"] in
		write_expr dest_path (fun () ->
			Opam_metadata.nix_of_opam ~cache ~deps ~name:package ~version path
		)
	);

	traverse_nix_files ~root:dest (fun package impls base ->
		let path_of_version = (fun ver -> `Lit ("./" ^ ver ^ ".nix")) in
		let path = Filename.concat base "default.nix" in
		write_expr path (fun () ->
			`Attrs (Nix_expr.AttrSet.build (
				("latest", impls |> latest_version |> path_of_version) ::
				(impls |> List.map (fun ver -> ver, path_of_version ver))
			))
		)
	);

	let () =
		let packages = list_dirs dest in
		let path_of_package = (fun p -> `Lit ("./" ^ p)) in
		let path = Filename.concat dest "default.nix" in
		write_expr path (fun () ->
			`Attrs (Nix_expr.AttrSet.build (
				packages |> List.map (fun ver -> ver, path_of_package ver)
			))
		)
	in

	(* Printf.eprintf "generated deps: %s\n" (deps#to_string) *)
	()
