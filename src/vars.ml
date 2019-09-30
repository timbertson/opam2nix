(* Logic for resolving opam vars.
 * Note that vars are used in multiple places:
 * Solve time: typically make use of global vars like ocaml version, platform, etc
 * Build / Install time: make use of self and dependency variables for locations and presence (typically by passing into configure scripts)
 *)

open Util
open OpamVariable

type package_installation = {
	path: string option;
	version: OpamPackage.Version.t option;
}

type package_implementation =
	| Provided
	| Installed of package_installation
	| Absent

type env =
	{
		packages: package_implementation StringMap.t;
		prefix: string option;
		self: string;
		vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	}

let implicit_package_var key =
	let open OpamVariable.Full in
	match (scope key, OpamVariable.to_string (variable key)) with
		| Package pkg, "installed" | Package pkg, "enabled" -> Some pkg
		| _ -> None

let path_var ~prefix ~scope key =
	(* global vars reference dirs inside the prefix (swtich) location, whereas scoped vars
	 * refer to the package dir within the above location *)
	let scope = ref scope in
	let relpath = match key with
		| "stublibs" | "toplevel" -> Some (Filename.concat "lib" key)
		| "bin" | "sbin" | "man" | "lib" | "libexec" | "etc" | "doc" | "share" -> Some key
		| "lib_root" | "share_root" -> scope := None; without_trailing "_root" key
		| _ -> None
	in
	relpath |> Option.map (fun relpath ->
		let base = Filename.concat prefix relpath in
		match !scope with
			| None -> S base
			| Some pkgname -> S (Filename.concat base pkgname)
	)

let package_var ~packages =
	let r_false = Some (B false) in
	let r_true = Some (B true) in
	let s v = Some (S v) in
	let b v = Some (B v) in
	let installed = function Absent -> false | Provided | Installed _ -> true in
	let rec lookup ~pkg key =
		debug "(variable `%s` of package `%s`)\n" key pkg;
		let impl = StringMap.find_opt pkg packages |> Option.default Absent in
		match key with
			| "installed" -> b (installed impl)
			| "enable" -> s (if (installed impl) then "enable" else "disable")
			| "name" -> s pkg
			| _ -> (match impl with
				| Provided -> lookup ~pkg:"ocaml" key
				| Absent -> None (* all vars aside from `installed` are undefined if the package is absent *)
				| Installed { path; version } -> (
					path
					|> Option.bind (fun path -> path_var ~prefix:path ~scope:(Some pkg) key)
					|> Option.or_else (match key with
						| "dev" -> r_false
						| "build-id" -> path |> Option.bind s (* nix paths are unique :) *)
						(* test and doc are undocumented, but appear in the wild... *)
						| "test" | "with-test" -> r_false
						| "doc" | "with-doc" -> r_false

						| "with-build" -> r_true (* dep used at build time. One day we'll remove these from PropagatedBuildInputs *)
						| "build" -> (
								(* Build acts as a filter when selecting, but as a path when building *)
								match path with
									| Some p -> s p
									| None -> r_true
							)
						| "post" -> r_false (* dep is to be installed after (or unrelated to) this package. Doesn't make much sense in nix *)
						| "pinned" -> r_false (* dep is to be installed after (or unrelated to) this package. Doesn't make much sense in nix *)
						| "version" -> version |> Option.map OpamPackage.Version.to_string |> Option.bind s
						| "hash" | "depends" ->
								(* no reasonable way to implement these, and no use cases reported *)
								s "NOT_IMPLEMENTED_IN_OPAM2NIX"
						| _ -> None
					)
				)
			)
	in
	lookup

let simple_lookup ~vars key =
	(* lookup in env or static set of variables *)
	Full.read_from_env key |> Option.or_else_fn (fun () ->
		try Some (Full.Map.find key vars)
		with Not_found -> None
	)

let lookup env key =
	let open OpamVariable in
	let keystr = (Full.to_string key) in
	let package_var = package_var ~packages:env.packages in
	debug "Looking up opam var %s ..\n" keystr;
	let result = simple_lookup ~vars:env.vars key |> Option.or_else_fn (fun () ->
		let unqualified = Full.variable key |> OpamVariable.to_string in
		match Full.scope key with
			| Full.Self -> package_var ~pkg:env.self unqualified
			| Full.Package pkg -> package_var ~pkg:(OpamPackage.Name.to_string pkg) unqualified
			| Full.Global -> (
				match keystr with
					| "prefix" | "switch" | "root" -> env.prefix |> Option.map (fun v -> S v)
					| "user" -> Some (S (Unix.getlogin ()))
					| "group" -> Unix.(
							try
								let gid = getgid () in
								let gname = (getgrgid gid).gr_name in
								Some (S gname)
							with Not_found -> None
					)
					| "sys-ocaml-version" ->
						(* delegate to the installed ocaml version *)
						package_var ~pkg:"ocaml" "version"
					| _ ->
						(* Try resolving remaining vars as scope-less directories,
						 * and then fallback to resolving a self-var. *)
						env.prefix |> Option.bind (fun prefix ->
							path_var ~prefix ~scope:None unqualified
						) |> Option.or_else_fn (fun () ->
							package_var ~pkg:env.self unqualified
						)
			)
	) in
	debug " -> %s\n" (Option.to_string OpamVariable.string_of_variable_contents result);
	if Option.is_none result then
		Printf.eprintf "WARN: opam var %s not found...\n" keystr;
	result
