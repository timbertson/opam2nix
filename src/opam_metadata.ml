open Util
module URL = OpamFile.URL
module OPAM = OpamFile.OPAM
module Descr = OpamFile.Descr
open OpamTypes
module StringSet = OpamStd.String.Set
module StringSetMap = OpamStd.String.SetMap

module StringMap = struct
	include Map.Make(String)
	let from_list items = List.fold_right (fun (k,v) map -> add k v map) items empty
end

type url = [
	| `http of string * (Digest_cache.opam_digest list)
	| `local of string
]

type url_type =
	[ `http
	| `git
	| `local
	| `darcs
	| `hg
	]

exception Unsupported_archive of string
exception Invalid_package of string
exception Checksum_mismatch of string
exception Not_cached of string

let var_prefix = "opam_var_"

type dependency =
	| NixDependency of string
	| SimpleOpamDependency of string
	| ExternalDependencies of (string list * OpamTypes.filter) list
	| PackageDependencies of OpamTypes.filtered_formula

type importance = Required | Optional
type requirement = importance * dependency

module ImportanceOrd = struct
	type t = importance

	let compare a b = match (a,b) with
		| Required, Required | Optional, Optional -> 0
		| Required, _ -> 1
		| Optional, _ -> -1

	let more_important a b = (compare a b) > 0
end

let string_of_relop : relop -> string = function
	| `Eq -> "="
	| `Geq -> ">="
	| `Gt -> ">"
	| `Leq -> "<="
	| `Lt -> "<"
	| `Neq -> "!="

let string_of_dependency = function
	| NixDependency dep -> "nix:"^dep
	| SimpleOpamDependency dep -> "package:"^dep
	| ExternalDependencies deps ->
			"external:" ^ (
				List.to_string (fun (deps, filter) ->
					(String.concat ",") deps ^ " {" ^ OpamFilter.to_string filter ^ "}"
				)
			) deps
	| PackageDependencies formula ->
		(* of OpamTypes.formula *)
		let string_of_filter : filter filter_or_constraint -> string = function
			| Filter f -> OpamFilter.to_string f
			| Constraint (op, f) -> (string_of_relop op) ^ (OpamFilter.to_string f)
		in
		let string_of_atom : (OpamPackage.Name.t * filter filter_or_constraint OpamFormula.formula) -> string = fun (name, formula) ->
			(OpamPackage.Name.to_string name) ^ ":" ^ (OpamFormula.string_of_formula string_of_filter formula)
		in
		"package-formula:" ^ (OpamFormula.string_of_formula string_of_atom formula)

let string_of_requirement = function
	| Required, dep -> string_of_dependency dep
	| Optional, dep -> "{" ^ (string_of_dependency dep) ^ "}"

let string_of_importance = function Required -> "required" | Optional -> "optional"

let add_var (scope: OpamVariable.t -> OpamVariable.Full.t) name v vars =
	let var: OpamVariable.Full.t = scope (OpamVariable.of_string name) in
	vars |> OpamVariable.Full.Map.add var v

let global_var = OpamVariable.Full.global
let add_global_var = add_var global_var
let package_var pkgname = OpamVariable.Full.create (OpamPackage.Name.of_string pkgname)
let add_package_var pkgname = add_var (package_var pkgname)

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

let nixpkgs_vars () =
	native_system_vars ()
		|> add_global_var "os-distribution" (S "nixpkgs")
		(* NixOS is more of a distribution, but for practical purposes
		 * "nixpkgs" defines what packages are available, regardless of distro *)
		|> add_global_var "os-version" (S "unknown")
			(* I don't think we can easily get a number here, but it should
			 * rarely matter *)

let add_base_variables base_vars =
	base_vars
		|> add_global_var "make" (S "make")
		|> add_global_var "opam-version" (S (OpamVersion.to_string OpamVersion.current))
		|> add_global_var "pinned" (B false) (* probably ? *)
		|> add_global_var "jobs" (S "1") (* XXX NIX_JOBS? *)
		(* With preinstalled packages suppose they can't write
		   in the ocaml directory *)
		|> add_global_var "preinstalled" (B true)
		|> add_package_var "ocaml" "preinstalled" (B true)
		|> add_package_var "ocaml" "native" (B true)
		|> add_package_var "ocaml" "native-tools" (B true)
		|> add_package_var "ocaml" "native-dynlink" (B true)

let init_variables () = add_base_variables (nixpkgs_vars ())

let installed_pkg_var key = let open OpamVariable in match Full.scope key with
	| Full.Package pkg when ((Full.variable key |> OpamVariable.to_string) = "installed") ->
			Some pkg
	| _ -> None

let rec lookup_var vars key =
	let open OpamVariable in
	let keystr = (Full.to_string key) in
	debug "Looking up opam var %s ..\n" keystr;
	let result =
		Full.read_from_env key |> Option.or_else_fn (fun () ->
		try Some (Full.Map.find key vars)
		with Not_found -> (
			let r_true = Some (B true) and r_false = Some (B false) in
			match Full.scope key with
				| Full.Self -> None
				| Full.Global ->
					if List.mem key OpamPackageVar.predefined_depends_variables then (
						match keystr with
						| "dev" -> r_false
						| "with-test" -> r_false
						| "with-doc" -> r_false
						| "build" -> r_true
						| _ -> None (* Computation delayed to the solver *)
					) else if keystr = "sys-ocaml-version" then (
						(* delegate to the installed ocaml version *)
						lookup_var vars (OpamVariable.Full.create (OpamPackage.Name.of_string "ocaml") (OpamVariable.of_string "version"))
					) else None
				| Full.Package pkg ->
					let pkg = OpamPackage.Name.to_string pkg in
					let unqualified = Full.variable key |> OpamVariable.to_string in
					debug "(variable `%s` of package `%s`)\n" unqualified pkg;
					if (installed_pkg_var key |> Option.is_some) then (
						(* evidently not... *)
						r_false
					) else None
		)
	) in
	debug " -> %s\n" (Option.to_string OpamVariable.string_of_variable_contents result);
	if Option.is_none result then
		Printf.eprintf "WARN: opam var %s not found...\n" keystr;
	result

let add_nix_inputs
	~(add_native: importance -> string -> unit)
	~(add_opam:importance -> string -> unit)
	importance dep =
	let desc = match importance with
		| Required -> "dep"
		| Optional -> "optional dep"
	in
	let nixpkgs_env = lookup_var (nixpkgs_vars ()) in
	debug "Adding dependency: %s\n" (string_of_dependency dep);
	match dep with
		| NixDependency name ->
				add_native importance name
		| SimpleOpamDependency dep -> add_opam importance dep
		| ExternalDependencies externals ->
				let apply_filters env (deps, filter) =
					try
						if (OpamFilter.eval_to_bool ~default:false env filter) then
							Some (deps)
						else
							None
					with Invalid_argument desc -> (
						Printf.eprintf "  Note: depext filter raised Invalid_argument: %s\n" desc;
						None
					)
				in

				let (importance, deps) =
					let nixpkgs_deps = filter_map (apply_filters nixpkgs_env) externals in
					if (nixpkgs_deps = []) then (
						debug "  Note: package has depexts, but none of them `nixpkgs`:\n    %s\n"
							(string_of_dependency dep);
						debug "  Adding them all as `optional` dependencies.\n";
						(Optional, List.map fst externals)
					) else (Required, nixpkgs_deps)
				in
				List.iter (fun deps ->
					List.iter (fun dep ->
						debug "  adding nix %s: %s\n" desc dep;
						add_native importance dep
					) deps
				) deps


		| PackageDependencies formula -> (
			let add importance (pkg, _version) = add_opam importance (OpamPackage.Name.to_string pkg) in
			let rec add_formula importance = let open OpamFormula in function
				| Empty    -> ()
				| Atom x   -> add importance x
				| Block x  -> add_formula importance x
				| And(x,y) -> add_formula importance x; add_formula importance y
				| Or(x,y) -> add_formula Optional x; add_formula Optional y
			in
			OpamPackageVar.filter_depends_formula
				~build:true
				~post:false
				~test:false
				~doc:false
				~default:false
				~env:nixpkgs_env
				formula |> add_formula importance;

			OpamPackageVar.filter_depends_formula
				~build:true
				~post:false
				~test:true
				~doc:true
				~default:true
				~env:nixpkgs_env
				formula |> add_formula Optional
		)

module PackageMap = OpamPackage.Map

class dependency_map =
	let map : requirement list PackageMap.t ref = ref PackageMap.empty in
	let get_existing package_id = try PackageMap.find package_id !map with Not_found -> [] in
	object
		method init_package package_id =
			let existing = get_existing package_id in
			map := PackageMap.add package_id existing !map

		method add_dep package_id (dep:requirement) =
			let existing = get_existing package_id in
			map := PackageMap.add package_id (dep :: existing) !map

		method to_string =
			let reqs_to_string = (fun reqs -> String.concat "," (List.map string_of_requirement reqs)) in
			PackageMap.to_string reqs_to_string !map
	end

let url urlfile: url =
	let (url, checksums) = URL.url urlfile, URL.checksum urlfile in
	let OpamUrl.({ hash; transport; backend; _ }) = url in
	let url_without_backend = OpamUrl.base_url url in
	let require_checksums checksums =
		if checksums = [] then
			raise (Unsupported_archive "Checksum required")
		else checksums
	in
	match (backend, transport, hash) with
	| `git, _, _ -> raise (Unsupported_archive "git")
	| `darcs, _, _ -> raise (Unsupported_archive "darcs")
	| `hg, _, _ -> raise (Unsupported_archive "hg")

	| `http, "file", None | `rsync, "file", None -> `local OpamUrl.(url.path)
	| `http, _, None -> `http (url_without_backend, require_checksums checksums) (* drop the VCS portion *)
	| `http, _, Some _ -> raise (Unsupported_archive "http with fragment")
	| `rsync, transport, None -> raise (Unsupported_archive ("rsync transport: " ^ transport))
	| `rsync, _, Some _ -> raise (Unsupported_archive "rsync with fragment")

let load_url path =
	if Sys.file_exists path then begin
		let url_file = open_in path in
		let rv = URL.read_from_channel url_file in
		close_in url_file;
		Some rv
	end else None

let load_opam path =
	(* Printf.eprintf "  Loading opam info from %s\n" path; *)
	if not (Sys.file_exists path) then raise (Invalid_package ("No opam file at " ^ path));
	let file = open_in path in
	let rv = OPAM.read_from_channel file in
	close_in file;
	OpamFormatUpgrade.opam_file rv

let concat_address (addr, frag) =
	match frag with
		| Some frag -> addr ^ "#" ^ frag
		| None -> addr

let string_of_url url =
	match url with
		| `http addr -> addr
		| `git addr -> "git:" ^ (concat_address addr)

let nix_of_url ~add_input ~cache ~offline (url:url) =
	let open Nix_expr in
	match url with
		| `local src -> `Lit src
		| `http (src, checksums) ->
			if (offline && not (Digest_cache.exists checksums cache)) then raise (Not_cached src);
			let digest = Digest_cache.add src checksums cache in
			add_input "fetchurl";
			`Call [
				`Id "fetchurl";
				`Attrs (AttrSet.build [
					"url", str src;
					(match digest with
						| `sha256 sha256 -> ("sha256", str sha256)
						| `checksum_mismatch desc -> raise (Checksum_mismatch desc)
					);
				])
			]

let unsafe_envvar_chars = Str.regexp "[^0-9a-zA-Z_]"
let envvar_of_ident name =
	var_prefix ^ (Str.global_replace unsafe_envvar_chars "_" name)

let add_implicit_build_dependencies ~add_dep commands =
	let implicit_optdeps = ref StringSet.empty in
	(* If your build command depends on foo:installed, you have an implicit optional
	 * build dependency on foo. Packages *should* declare this, but don't always... *)
	let lookup_var key =
		match installed_pkg_var key with
			| None -> None
			| Some pkg ->
				let pkgname = OpamPackage.Name.to_string pkg in
				debug "  adding implied dep: %s\n" pkgname;
				implicit_optdeps := !implicit_optdeps |> StringSet.add pkgname;
				Some (B true)
	in
	commands |> List.iter (fun commands ->
		let (_:string list list) = commands |> OpamFilter.commands lookup_var in
		()
	);
	!implicit_optdeps |> StringSet.iter (fun pkg ->
		add_dep Optional (SimpleOpamDependency pkg)
	)
;;

let attrs_of_opam ~add_dep (opam:OPAM.t) =
	add_implicit_build_dependencies ~add_dep [OPAM.build opam; OPAM.install opam];
	add_dep Optional (PackageDependencies (OPAM.depopts opam));
	add_dep Required (PackageDependencies (OPAM.depends opam));
	let depexts = OPAM.depexts opam in
	if (depexts <> []) then add_dep Required (ExternalDependencies depexts);
	[
		"configurePhase",  Nix_expr.str "true"; (* configuration is done in build commands *)
		"buildPhase",      `Lit "\"${opam2nix}/bin/opam2nix invoke build\"";
		"installPhase",    `Lit "\"${opam2nix}/bin/opam2nix invoke install\"";
	]
;;

module InputMap = struct
	include StringMap
	(* override `add` to keep the "most required" entry *)
	let add k v map =
		let existing = try Some (find k map) with Not_found -> None in
		match existing with
			| Some existing when (ImportanceOrd.more_important existing v) -> map
			| _ -> add k v map
end

let nix_of_opam ~name ~version ~cache ~offline ~deps ~has_files path : Nix_expr.t =
	let pkgid = OpamPackage.create
		(OpamPackage.Name.of_string name)
		(Repo.opam_version_of version)
	in
	let open Nix_expr in
	let pkgs_expression_inputs = ref (InputMap.from_list [
		"lib", Required;
	]) in
	let additional_env_vars = ref [] in
	let adder r = fun importance name -> r := InputMap.add name importance !r in

	deps#init_package pkgid;

	let opam_inputs = ref InputMap.empty in
	let nix_deps = ref InputMap.empty in
	let add_native = adder nix_deps in
	let add_opam_input = adder opam_inputs in
	let add_expression_input = adder pkgs_expression_inputs in

	let is_conf_pkg pkg =
		let re = Str.regexp "^conf-" in
		Str.string_match re pkg 0
	in

	(* If ocamlfind is in use by _anyone_ make it used by _everyone_. Otherwise,
	 * we end up with inconsistent install paths. XXX this is a bit hacky... *)
	if not (name = "ocamlfind" || is_conf_pkg name)
		then add_opam_input Optional "ocamlfind";
	add_opam_input Required "ocaml"; (* pretend this is an `opam` input for convenience *)

	let add_dep = fun importance dep ->
		add_nix_inputs
			~add_native
			~add_opam:add_opam_input
			importance dep
	in

	let opam = load_opam (Filename.concat path "opam") in

	let urlfile = match OPAM.url opam with
		| Some _ as url -> url
		| None -> load_url (Filename.concat path "url")
	in
	let url = urlfile |> Option.map (fun urlfile ->
		try url urlfile
		with Unsupported_archive reason -> raise (
			Unsupported_archive (name ^ "-" ^ (Repo.string_of_version version) ^ ": " ^ reason)
		)
	) in

	let src = Option.map (
		nix_of_url ~add_input:(add_expression_input Required) ~cache ~offline
	) url in

	let buildAttrs : (string * Nix_expr.t) list = attrs_of_opam ~add_dep opam in

	let url_ends_with ext = (match url with
		| Some (`http (url,_)) | Some (`local url) -> ends_with ext url
		| _ -> false
	) in

	if url_ends_with ".zip" then add_native Required "unzip";

	let property_of_input src (name, importance) : Nix_expr.t =
		match importance with
			| Optional -> `Property_or (src, name, `Null)
			| Required -> `PropertyPath (src, String.split_on_char '.' name)
	in
	let attr_of_input src (name, importance) : string * Nix_expr.t =
		(name, property_of_input src (name, importance))
	in
	let sorted_bindings_of_input input = input
		|> InputMap.bindings
		|> List.sort (fun (a,_) (b,_) -> String.compare a b)
	in

	let opam_inputs : Nix_expr.t AttrSet.t =
		!opam_inputs |> InputMap.mapi (fun name importance ->
			property_of_input (`Id "selection") (name, importance)) in

	let nix_deps = !nix_deps
		|> sorted_bindings_of_input
		|> List.map (property_of_input (`Id "pkgs"))
	in
	let expression_args : (string * Nix_expr.t) list = !pkgs_expression_inputs
		|> sorted_bindings_of_input
		|> List.map (attr_of_input (`Id "pkgs"))
	in
	let version_str = Repo.path_of_version `Nix version in

	`Function (
		`Id "world",
		`Let_bindings (
			(AttrSet.build ([
				"lib", `Lit "world.pkgs.lib";
				"selection", `Property (`Id "world", "selection");
				"opam2nix", `Property (`Id "world", "opam2nix");
				"pkgs", `Property (`Id "world", "pkgs");
				"opamDeps", `Attrs opam_inputs;
				"inputs", `Call [
						`Id "lib.filter";
						`Lit "(dep: dep != true && dep != null)";
						`BinaryOp (
							`List nix_deps,
							"++",
							`Lit "(lib.attrValues opamDeps)"
						);
				];
			] @ expression_args) ),
			`Call [
				`Id "pkgs.stdenv.mkDerivation";
				`Attrs (AttrSet.build (!additional_env_vars @ [
					"name", Nix_expr.str (name ^ "-" ^ version_str);
					"version", Nix_expr.str version_str;
					"opamEnv", `Call [`Id "builtins.toJSON"; `Attrs (AttrSet.build [
						"spec", `Lit "./opam";
						"deps", `Lit "opamDeps";
						"name", Nix_expr.str name;
						"files", if has_files then `Lit "./files" else `Null;
						"ocaml-version", `Property (`Id "world", "ocamlVersion");
					])];
					"buildInputs", `Lit "inputs";
					(* TODO: don't include build-only deps *)
					"propagatedBuildInputs", `Lit "inputs";
					"passthru", `Attrs (AttrSet.build [
						"selection", `Id "selection";
					]);
				] @ (
					if has_files
						then [
							"prePatch", `String [`Lit "cp -r "; `Expr (`Lit "./files"); `Lit "/* ./" ]
						]
						else []
				) @ buildAttrs @ (
					match src with
						| Some src -> [ "src", src ]
						| None -> [ "unpackPhase", str "true" ]
				) @ (
					if url_ends_with ".tbz" then
						["unpackCmd", Nix_expr.str "tar -xf \"$curSrc\""]
					else []
				)))
			]
		)
	)
