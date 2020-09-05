open Util
open OpamFile
module Version = OpamPackage.Version
module Name = OpamPackage.Name
module Seq = Seq_ext

let (%) f g x = f (g x)

type version = OpamPackage.Version.t

type repo_url =
	| Github of (string * string)
	| Url of string

type spec = {
	github_owner: string;
	github_name: string;
	spec_commit: string option;
}

type t = {
	repo_key: string;
	spec: spec;
	(* TODO: OpamFilename.Dir.t? *)
	repo_path: string;
	repo_commit: string;
	(* returned as Lwt.t because it's needed lazily much later than commit and version *)
	repo_digest: Digest_cache.nix_digest Lwt.t;
}

module PackageSet = OpamPackage.Set

let decreasing_version_order versions =
	let compare a b =
		(* Note: we invert this because we want a descending list *)
		OpamPackage.Version.compare a b
	in
	versions |> List.sort compare

let latest_version versions = List.hd (decreasing_version_order versions)

type package = {
	repo: t;
	package: OpamPackage.t;
	rel_path: string;

	(* TODO Make these lazy? *)
	opam: OPAM.t;
	url: URL.t option;
}

let packages_dir = "packages"

let full_path pkg = Filename.concat pkg.repo.repo_path pkg.rel_path

let git_url spec = Printf.sprintf "https://github.com/%s/%s.git" spec.github_owner spec.github_name

let package_desc pkg = OpamPackage.to_string pkg.package

let load_url path =
	if Sys.file_exists path then begin
		let url_file = open_in path in
		let rv = URL.read_from_channel url_file in
		close_in url_file;
		Some rv
	end else None


let list_package =
	let version_sep = "." in
	let version_join package version =
		package ^ version_sep ^ (Version.to_string version) in
	fun repo package -> (
		debug "processing package %s\n" package;
		let package_base = Filename.concat packages_dir package in
		let package_abs = Filename.concat repo.repo_path package_base in
		let list_versions () =
			debug "listing %s\n" package_abs;
			let dirs = list_dirs package_abs in
			dirs |> filter_map (fun ent ->
				if Sys.file_exists (Filename.concat(Filename.concat package_abs ent) "opam")
					then Some (Version.of_string ent)
					else (Printf.eprintf "Skipping non-package directory %s ()\n" ent; None)
			)
		in

		list_versions () |> List.map (fun version ->
			let rel_path = Filename.concat package_base (version_join package version) in
			let full_path = Filename.concat repo.repo_path rel_path in
			let opam = Opam_metadata.load_opam (Filename.concat full_path "opam") in
			{
				repo;
				package = OpamPackage.create (Name.of_string package) version;
				(* TODO is rel_path still needed? *)
				rel_path;
				opam;
				url = OPAM.url opam |> Option.or_else (load_url (Filename.concat full_path "url"));
			}
		)
	)

let traverse ~repos : package Seq.t =
	let filter =
		let seen = ref PackageSet.empty in
		fun package ->
			if PackageSet.mem package.package !seen then (
				Printf.eprintf "Skipping %s (already loaded %s)\n" package.rel_path (package_desc package);
				false
			) else (
				seen := PackageSet.add package.package !seen;
				debug "Processing package %s\n" package.rel_path;
				true
			)
	in

	let traverse_repo repo = (
		list_dirs (Filename.concat repo.repo_path packages_dir)
			|> Seq.of_list
			|> Seq.flat_map (fun pkg -> list_package repo pkg |> Seq.of_list)
	) in
	repos
		|> Seq.of_list
		|> Seq.flat_map traverse_repo
		|> Seq.filter filter
