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

let ocaml_name = Name.of_string "ocaml"

(* Information about a pacakge.
 * Post-solve, only name and version are known.
 * Path is only known at build time (i.e. during Invoke)
 *)
type selected_package = {
	name: Name.t;
	sel_version: Version.t option;
	path: OpamFilename.Dir.t option;
}

(* Pre-solve, only `ocaml` package is present, everything else comes from `vars *)
type state = {
	st_packages: selected_package Name.Map.t;
	st_vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	ocaml_version: Version.t option;
	(* Only set to true from invoke, otherwise unix user & group will not be undefined *)
	is_building: bool;
	st_partial: bool; (* false simply suppressed unresolved warnings *)
}

let add_var (scope: OpamVariable.t -> OpamVariable.Full.t) name v vars =
	let var: OpamVariable.Full.t = scope (OpamVariable.of_string name) in
	vars |> OpamVariable.Full.Map.add var v

let package_var pkgname = OpamVariable.Full.create (OpamPackage.Name.of_string pkgname)
let global_var = OpamVariable.Full.global
let self_var = OpamVariable.Full.self
let add_global_var = add_var global_var
let add_package_var pkgname = add_var (package_var pkgname)
let add_self_var name = add_var self_var name

let native_system_vars () =
	let state = OpamVariable.Full.Map.empty in
	let system_variables = OpamSysPoll.variables in
	List.fold_left
		(fun state ((name: OpamVariable.t), value) ->
			(Lazy.force value) |> Option.map (fun value ->
				OpamVariable.Full.Map.add (OpamVariable.Full.global name) value state
			) |> Option.default state
		)
		state system_variables

let nixos_vars () =
	native_system_vars ()
		|> add_global_var "os-family" (S "unknown")
		|> add_global_var "os-distribution" (S "nixos")
		|> add_global_var "os-version" (S "unknown")
			(* I don't think we can easily get a number here, but it should
			 * rarely matter *)

let init_variables ~is_building () =
	let common = nixos_vars ()
		|> add_global_var "make" (S "make")
		|> add_global_var "opam-version" (S (OpamVersion.to_string OpamVersion.current))
		|> add_global_var "pinned" (B false) (* probably ? *)
		|> add_global_var "enable-ocaml-beta-repository" (B false)
		(* With preinstalled packages suppose they can't write
			 in the ocaml directory *)
		|> add_global_var "preinstalled" (B true)
		|> add_package_var "ocaml" "preinstalled" (B true)
		|> add_package_var "ocaml" "native" (B true)
		|> add_package_var "ocaml" "native-tools" (B true)
		|> add_package_var "ocaml" "native-dynlink" (B true)
	in
	if is_building
		then common |> add_global_var "jobs" (S (getenv_opt "NIX_BUILD_CORES" |> Option.default "1"))
		else common

let state ~is_building ?(partial=true) packages =
	let ocaml_version = Name.Map.find_opt ocaml_name packages
		|> Option.bind (fun ocaml -> ocaml.sel_version)
	in
	{
		is_building;
		ocaml_version;
		st_partial = partial;
		st_packages = packages;
		st_vars = init_variables ~is_building ();
	}

let string_of_dir = OpamFilename.Dir.to_string

let string_of_selected_package { name; sel_version; path } = Printf.sprintf "{ name = %s; version = %s; path = %s }"
	(Name.to_string name)
	(Option.to_string Version.to_string sel_version)
	(Option.to_string OpamFilename.Dir.to_string path)

let selected_package ?version ?path name = { name; sel_version = version; path }

let implicit_package_var key =
	let open OpamVariable.Full in
	match (scope key, OpamVariable.to_string (variable key)) with
		| Package pkg, "installed" | Package pkg, "enabled" -> Some pkg
		| _ -> None

let _path_var ~ocaml_version ~prefix ~scope key =
	(* global vars reference dirs inside the prefix (swtich) location, whereas scoped vars
	 * refer to the package dir within the above location *)
	let scope = ref scope in
	let lib_rel () = "lib/ocaml/" ^ (ocaml_version |> Version.to_string) ^ "/site-lib" in
	let rec relpath key = match key with
		| "lib" -> Some (lib_rel ())
		| "stublibs" | "toplevel" -> Some (Filename.concat (lib_rel ()) key)
		| "bin" | "sbin" | "man" | "libexec" | "etc" | "doc" | "share" -> Some key
		| "lib_root" | "share_root" -> (
			scope := None;
			without_trailing "_root" key |> Option.bind relpath
		)
		| _ -> None
	in
	relpath key |> Option.map (fun relpath ->
		let base = prefix / relpath in
		S (OpamFilename.Dir.to_string (match !scope with
			| None -> base
			| Some pkgname -> (prefix / relpath) / (Name.to_string pkgname)
		))
	)

let path_var ~ocaml_version ~prefix ~scope key = ocaml_version |> Option.bind
	(fun ocaml_version -> _path_var ~ocaml_version ~prefix ~scope key)

let package_var : state -> Name.t -> string -> variable_contents option =
	let r_false = Some (B false) in
	let r_true = Some (B true) in
	let s v = Some (S v) in
	let b v = Some (B v) in
	let installed = Option.is_some in
	fun state pkg key ->
		debug "(variable `%s` of package `%s`)\n" key (Name.to_string pkg);
		let impl = Name.Map.find_opt pkg state.st_packages in
		match key with
			| "installed" -> b (installed impl)
			| "enable" -> s (if (installed impl) then "enable" else "disable")
			| "name" -> s (Name.to_string pkg)
			| _ -> impl |> Option.bind (fun { sel_version; path; _ } ->
				path
				|> Option.bind (fun path ->
					path_var ~ocaml_version:state.ocaml_version ~prefix:path ~scope:(Some pkg) key
				)
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
					| "version" -> sel_version |> Option.map OpamPackage.Version.to_string |> Option.bind s
					| "hash" | "depends" ->
							(* no reasonable way to implement these, and no use cases reported *)
							s "NOT_IMPLEMENTED_IN_OPAM2NIX"
					| _ -> None
				)
			)


let prefix : state -> Name.t -> OpamFilename.Dir.t option = fun state name ->
	Name.Map.find_opt name state.st_packages |> Option.bind (fun sel -> sel.path)

let simple_lookup ~vars key =
	(* lookup in env or static set of variables *)
	Full.read_from_env key |> Option.or_else_fn (fun () ->
		try Some (Full.Map.find key vars)
		with Not_found -> None
	)

let lookup (state: state) ~(self: Name.t option) key =
	let open OpamVariable in
	let keystr = (Full.to_string key) in
	let package_var = package_var state in
	debug "Looking up opam var %s ..\n" keystr;
	let result = simple_lookup ~vars:state.st_vars key |> Option.or_else_fn (fun () ->
		let unqualified = Full.variable key |> OpamVariable.to_string in
		match Full.scope key with
			| Full.Self -> self |> Option.bind (fun self -> package_var self unqualified)
			| Full.Package pkg -> package_var pkg unqualified
			| Full.Global -> (
				match keystr with
					| "prefix" | "switch" | "root" ->
						self |> Option.bind (prefix state) |> Option.map (fun v -> S (string_of_dir v))
					| "user" -> if state.is_building then Some (S (Unix.getlogin ())) else None
					| "group" -> if state.is_building then Unix.(
							try
								let gid = getgid () in
								let gname = (getgrgid gid).gr_name in
								Some (S gname)
							with Not_found -> None
						) else None
					| "sys-ocaml-version" ->
						(* delegate to the installed ocaml version *)
						package_var (Name.of_string "ocaml") "version"
					| _ ->
						(* Try resolving remaining vars as scope-less directories,
						 * and then fallback to resolving a self-var. *)
						self |> Option.bind (fun self ->
							prefix state self |> Option.bind (fun prefix ->
								path_var ~ocaml_version:state.ocaml_version ~prefix ~scope:None unqualified
							) |> Option.or_else_fn (fun () ->
								package_var self unqualified
							)
						)
			)
	) in
	debug " -> %s\n" (Option.to_string OpamVariable.string_of_variable_contents result);
	if (not state.st_partial) && Option.is_none result then
		Printf.eprintf "WARN: opam var %s not found...\n" keystr;
	result