open Printf

let rec filter_map fn lst =
	lst |> List.fold_left (fun acc item ->
		match fn item with
			| None -> acc
			| Some result -> result :: acc
	) [] |> List.rev


let traverse_repo ~repo ~packages emit =
	let sep = Str.regexp "@" in
	let pkgroot = Filename.concat repo "packages" in
	let packages = match packages with
		| [] -> Sys.readdir pkgroot |> Array.to_list
		| packages -> packages
	in

	let version_sep = "." in
	packages |> List.iter (fun spec ->
		let (package, version) = match Str.split sep spec with
			| [package] -> (package, None)
			| [package; version] -> (package, Some version)
			| _ -> failwith "Invalid package specifier"
		in
		let package_base = Filename.concat pkgroot package in
		let versions = match version with
			| Some version -> [version]
			| None ->
				let prefix = package ^ version_sep in
				let prefix_len = String.length prefix in
				Sys.readdir package_base |> Array.to_list |> filter_map (fun ent ->
					let len = String.length ent in
					if len > prefix_len && String.sub ent 0 prefix_len = prefix
						then (
							let version_len = len - prefix_len in
							Some (String.sub ent prefix_len version_len)
						) else (
							Printf.eprintf "Skipping non-package directory %s ()\n" ent;
							None
						)
				)
		in
		versions |> List.iter (fun version ->
			let path = Filename.concat package_base (package ^ version_sep ^ version) in
			emit package version path
		)
	)

let () =
	let repo = ref "" in
	let dest = ref "" in
	let cache = ref "" in
	let opts = Arg.align [
		("--repo", Arg.Set_string repo, "Repository root");
		("--dest", Arg.Set_string dest, "Destination (must not exist)");
		("--cache", Arg.Set_string cache, "Cache (may exist)");
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
		Unix.rmdir dest;
		mkdir dest
	) in

	let () = try
		mkdir cache
	with Unix.Unix_error(Unix.EEXIST, _, _) -> () in

	let cache = new File_cache.cache ~max_age:100000 cache in

	let deps = new Opam_metadata.dependency_map in

	traverse_repo ~repo ~packages (fun package version path ->
		let dest_path = Filename.concat dest (package ^ "-" ^ version ^ ".nix") in
		Printf.eprintf "Generating %s ...\n" dest_path;
		let expr = Opam_metadata.nix_of_opam ~cache ~deps ~name:package ~version path in
		let oc = open_out dest_path in
		Nix_expr.write oc expr;
		close_out oc
	);
	Printf.eprintf "generated deps: %s\n" (deps#to_string)
