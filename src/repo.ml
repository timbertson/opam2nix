open Util
module Version = OpamPackage.Version
module Name = OpamPackage.Name
module Seq = Seq_ext

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

let traverse ~repos : package Seq.t =
	let version_sep = "." in
	let version_join package version =
		package ^ version_sep ^ (Version.to_string version) in
	let filter =
		let seen = ref PackageSet.empty in
		fun package ->
			if PackageSet.mem package.package !seen then (
				Printf.eprintf "Skipping %s (already loaded %s)\n" package.path (package_desc package);
				false
			) else (
				seen := PackageSet.add package.package !seen;
				debug "Processing package %s\n" package.path;
				true
			)
	in

	let traverse_package ~packages_base ~repo package = (
		debug "processing package %s\n" package;
		let package_base = Filename.concat packages_base package in
		let package_abs = Filename.concat repo package_base in
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

		list_versions () |> Seq.of_list |> Seq.map (fun version ->
			let path = Filename.concat package_base (version_join package version) in
			let package = OpamPackage.create (Name.of_string package) version in
			{
				repo_base = repo;
				package; path;
			}
		)
	) in

	let traverse_repo repo = (
		let packages_base = "packages" in
		list_dirs (Filename.concat repo packages_base)
			|> Seq.of_list
			|> Seq.flat_map (traverse_package ~repo ~packages_base)
	) in
	repos
		|> Seq.of_list
		|> Seq.flat_map traverse_repo
		|> Seq.filter filter
