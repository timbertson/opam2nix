open Util
module Version = OpamPackage.Version
module Name = OpamPackage.Name
module OPAM = OpamFile.OPAM

let (%) f g x = f (g x)

type version = OpamPackage.Version.t

module PackageSet = OpamPackage.Set

let decreasing_version_order versions =
	let compare a b =
		(* Note: we invert this because we want a descending list *)
		OpamPackage.Version.compare a b
	in
	versions |> List.sort compare

let latest_version versions = List.hd (decreasing_version_order versions)

type package = {
	repo_base: string;
	opam: OPAM.t;
	path: string;
	package: OpamPackage.t;
}

let package_path pkg = Filename.concat pkg.repo_base pkg.path

let package_desc pkg = OpamPackage.to_string pkg.package

let version_sep = "."
let list_versions ~name_str patn =
	debug "listing %s\n" patn;
	let dirs = list_dirs patn in
	let prefix = name_str ^ version_sep in
	let versions = dirs |> filter_map (fun ent ->
		match ent |> without_leading prefix with
			| None -> Printf.eprintf "Skipping non-package directory %s ()\n" ent; None
			| x -> x
	) in
	List.map Version.of_string versions

let traverse_directed ~repos ~opams ~(names:Name.t list) (emit: package -> unit) =
	let emit =
		let seen = ref PackageSet.empty in fun package ->
		if PackageSet.mem package.package !seen then
			Printf.eprintf "Skipping %s (already loaded %s)\n" package.path (package_desc package)
		else begin
			seen := PackageSet.add package.package !seen;
			debug "Processing package %s\n" package.path;
			try
				emit package
			with e -> (
				Printf.eprintf "Error raised while processing %s:\n" package.path;
				raise e
			)
		end
	in

	let deps_of_opam : OPAM.t -> Name.Set.t = fun opam ->
		let names = ref Name.Set.empty in
		let add_name = fun (name, _) ->
			names := Name.Set.add name !names
		in
		OPAM.depends opam |> OpamFormula.iter add_name;
		OPAM.depopts opam |> OpamFormula.iter add_name;
		!names
	in

	let version_join package version =
		package ^ version_sep ^ (Version.to_string version) in
	let packages_base = "packages" in

	let rec loop ~seen_names ~todo = (
		Name.Set.choose_opt todo |> Option.may (fun name ->
			let todo = ref (Name.Set.remove name todo) in
			let seen_names = Name.Set.add name seen_names in
			let name_str = Name.to_string name in

			debug "processing package %s\n" name_str;
			repos |> List.iter (fun repo ->
				let abs = Filename.concat repo in
				let package_base = Filename.concat packages_base name_str in
				let package_abs = abs package_base in
				if Sys.file_exists package_abs then (
					list_versions ~name_str package_abs |> List.iter (fun version ->
						let path = Filename.concat package_base (version_join name_str version) in
						let package = OpamPackage.create (Name.of_string name_str) version in
						let opam = Opam_metadata.load_opam (abs (Filename.concat path "opam")) in
						(* traverse all deps we haven't yet done *)
						deps_of_opam opam |> Name.Set.iter (fun name ->
							if not (Name.Set.mem name seen_names) then
								todo := Name.Set.add name !todo
						);
						emit {
							repo_base = repo;
							opam; package; path;
						}
					)
				) else ()
			);
			loop ~seen_names ~todo:!todo
		)
	) in

	let named_specs = names |> Name.Set.of_list in
	let opam_deps = opams |> List.map deps_of_opam |> List.fold_left Name.Set.union Name.Set.empty in
	loop ~seen_names:Name.Set.empty ~todo:(Name.Set.union named_specs opam_deps)

