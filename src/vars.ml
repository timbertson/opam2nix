(* Logic for resolving opam vars.
 * Note that vars are used in multiple places:
 * Solve time: typically make use of global vars like ocaml version, platform, etc
 * Build / Install time: make use of self and dependency variables for locations and presence (typically by passing into configure scripts)
 *)

open Util
open OpamVariable
open OpamFilename.Op
module Name = OpamPackage.Name
module Version = OpamPackage.Version

let string_of_dir = OpamFilename.Dir.to_string

type package_installation = {
	path: OpamFilename.Dir.t option;
	version: OpamPackage.Version.t option;
}

type package_implementation =
	| Provided
	| Installed of package_installation
	| Absent

type env =
	{
		packages: package_implementation Name.Map.t;
		prefix: OpamFilename.Dir.t option;
		ocaml_version: OpamPackage.Version.t;
		self: Name.t;
		vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	}

let implicit_package_var key =
	let open OpamVariable.Full in
	match (scope key, OpamVariable.to_string (variable key)) with
		| Package pkg, "installed" | Package pkg, "enabled" -> Some pkg
		| _ -> None

module NixLayout = struct
	type ctx = env
	let root d _ = d
	let lib_dir d env = ((d / "lib") / ("ocaml" ^ (env.ocaml_version |> Version.to_string))) / "site-lib"
end

module PathDefault = OpamPath.Switch.DefaultF(NixLayout: OpamPath.LAYOUT)

let path_var ~env ~prefix ~scope key : variable_contents option =
	(* global vars reference dirs inside the prefix (swtich) location, whereas scoped vars
	 * refer to the package dir within the above location *)
	let open PathDefault in
	let fallback pkgscope global = Some (
		scope |> Option.map (pkgscope prefix env) |> Option.default (global prefix env)
	) in
	let global fn = Some (fn prefix env) in
	let path = match key with
		(* global or scoped *)
		| "lib" | "libexec" -> fallback lib lib_dir
		| "etc" -> fallback etc etc_dir
		| "share" -> fallback share share_dir
		| "doc" -> fallback doc doc_dir
		(* global-only *)
		| "bin" -> global bin
		| "man" -> global man_dir
		| "toplevel" -> global toplevel
		| "stublibs" -> global stublibs
		| "lib_root" -> global lib_dir
		| "share_root" -> global share_dir
		| _ -> None
	in
	path |> Option.map (fun p -> S (OpamFilename.Dir.to_string p))

let package_var ~env =
	let r_false = Some (B false) in
	let r_true = Some (B true) in
	let s v = Some (S v) in
	let b v = Some (B v) in
	let installed = function Absent -> false | Provided | Installed _ -> true in
	let rec lookup ~pkg key =
		debug "(variable `%s` of package `%s`)\n" key (Name.to_string pkg);
		let impl = Name.Map.find_opt pkg env.packages |> Option.default Absent in
		match key with
			| "installed" -> b (installed impl)
			| "enable" -> s (if (installed impl) then "enable" else "disable")
			| "name" -> s (Name.to_string pkg)
			| _ -> (match impl with
				| Provided -> lookup ~pkg:(Name.of_string "ocaml") key
				| Absent -> None (* all vars aside from `installed` are undefined if the package is absent *)
				| Installed { path; version } -> (
					path
					|> Option.bind (fun path -> path_var ~env ~prefix:path ~scope:(Some pkg) key)
					|> Option.or_else (match key with
						| "dev" -> r_false
						| "build-id" -> path |> Option.map string_of_dir |> Option.bind s (* nix paths are unique :) *)
						(* test and doc are undocumented, but appear in the wild... *)
						| "test" | "with-test" -> r_false
						| "doc" | "with-doc" -> r_false

						| "with-build" -> r_true (* dep used at build time. One day we'll remove these from PropagatedBuildInputs *)
						| "build" -> (
								(* Build acts as a filter when selecting, but as a path when building *)
								match path with
									| Some p -> s (string_of_dir p)
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
	let package_var = package_var ~env in
	debug "Looking up opam var %s ..\n" keystr;
	let result = simple_lookup ~vars:env.vars key |> Option.or_else_fn (fun () ->
		let unqualified = Full.variable key |> OpamVariable.to_string in
		match Full.scope key with
			| Full.Self -> package_var ~pkg:env.self unqualified
			| Full.Package pkg -> package_var ~pkg:pkg unqualified
			| Full.Global -> (
				match keystr with
					| "prefix" | "switch" | "root" -> env.prefix |> Option.map (fun v -> S (string_of_dir v))
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
						package_var ~pkg:(Name.of_string "ocaml") "version"
					| _ ->
						(* Try resolving remaining vars as scope-less directories,
						 * and then fallback to resolving a self-var. *)
						env.prefix |> Option.bind (fun prefix ->
							path_var ~env ~prefix ~scope:None unqualified
						) |> Option.or_else_fn (fun () ->
							package_var ~pkg:env.self unqualified
						)
			)
	) in
	debug " -> %s\n" (Option.to_string OpamVariable.string_of_variable_contents result);
	if Option.is_none result then
		Printf.eprintf "WARN: opam var %s not found...\n" keystr;
	result
