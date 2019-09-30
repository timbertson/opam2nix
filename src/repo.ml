open Util
module Version = OpamPackage.Version
module Name = OpamPackage.Name

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
	path: string;
	package: OpamPackage.t;
}

let package_path pkg = Filename.concat pkg.repo_base pkg.path

let package_desc pkg = OpamPackage.to_string pkg.package

let traverse ~repos (emit: package -> unit) =
	let version_sep = "." in
	let version_join package version =
		package ^ version_sep ^ (Version.to_string version) in
	let seen = ref PackageSet.empty in
	let emit package =
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

	repos |> List.iter (fun repo ->
		let abs = Filename.concat repo in
		let packages_base = "packages" in

		let process_package package =
			debug "processing package %s\n" package;
			let package_base = Filename.concat packages_base package in
			let package_abs = abs package_base in
			let list_versions () =
				debug "listing %s\n" package_abs;
				let dirs = list_dirs package_abs in
				let prefix = package ^ version_sep in
				let versions = dirs |> filter_map (fun ent ->
					match ent |> without_leading prefix with
						| None -> Printf.eprintf "Skipping non-package directory %s ()\n" ent; None
						| x -> x
				) in
				List.map Version.of_string versions
			in

			list_versions () |> List.iter (fun version ->
				let path = Filename.concat package_base (version_join package version) in
				let package = OpamPackage.create (Name.of_string package) version in
				emit {
					repo_base = repo;
					package; path;
				}
			)
		in

		list_dirs (abs packages_base) |> List.iter process_package
	)
