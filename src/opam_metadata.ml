open Util
module URL = OpamFile.URL
module OPAM = OpamFile.OPAM
module Descr = OpamFile.Descr
open OpamTypes
module StringSet = OpamStd.String.Set
module StringSetMap = OpamStd.String.SetMap

type url = [
	| `http of string * (Digest_cache.opam_digest list)
]
let string_of_url : url -> string = function
	| `http (url, _digest) -> url

type url_type =
	[ `http
	| `git
	| `darcs
	| `hg
	]

type unsupported_archive = [ `unsupported_archive of string ]
let string_of_unsupported_archive : unsupported_archive -> string =
	function (`unsupported_archive msg) -> "Unsupported archive: " ^ msg

let url_to_yojson (`http (url, _digests)) =
	`Assoc [
		url, `String url;
		(* TODO: digests *)
	]

exception Invalid_package of string

let var_prefix = "opam_var_"

type dependency =
	| NixDependency of string
	| SimpleOpamDependency of string
	| ExternalDependencies of (OpamSysPkg.Set.t * OpamTypes.filter) list
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
					let names = deps |> OpamSysPkg.Set.elements |> List.map OpamSysPkg.to_string in
					String.concat "," names ^ " {" ^ OpamFilter.to_string filter ^ "}"
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

let add_nix_inputs
	~(add_native: importance -> string -> unit)
	~(add_opam:importance -> string -> unit)
	importance dep =
	let desc = match importance with
		| Required -> "dep"
		| Optional -> "optional dep"
	in
	let nixos_env = Vars.(simple_lookup ~vars:(nixos_vars ())) in
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
					let nixos_deps = filter_map (apply_filters nixos_env) externals in
					if (nixos_deps = []) then (
						debug "  Note: package has depexts, but none of them `nixos`:\n    %s\n"
							(string_of_dependency dep);
						debug "  Adding them all as `optional` dependencies.\n";
						(Optional, List.map fst externals)
					) else (Required, nixos_deps)
				in
				List.iter (fun deps ->
					OpamSysPkg.Set.iter (fun dep ->
						let name = OpamSysPkg.to_string dep in
						debug "  adding nix %s: %s\n" desc name;
						add_native importance name
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
				~env:nixos_env
				formula |> add_formula importance;

			OpamPackageVar.filter_depends_formula
				~build:true
				~post:false
				~test:false
				~doc:false
				~default:false
				~env:nixos_env
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

let url urlfile: (url, [> unsupported_archive]) Result.t =
	let (url, checksums) = URL.url urlfile, URL.checksum urlfile in
	let OpamUrl.({ hash; transport; backend; _ }) = url in
	let url_without_backend = OpamUrl.base_url url in
	let checksums =
		if checksums = [] then
			Error (`unsupported_archive "Checksum required")
		else Ok checksums
	in
	match (backend, transport, hash) with
	| `git, _, _ -> Error (`unsupported_archive "git")
	| `darcs, _, _ -> Error (`unsupported_archive "darcs")
	| `hg, _, _ -> Error (`unsupported_archive "hg")

	| `http, "file", None | `rsync, "file", None -> Error (`unsupported_archive "local path")
	| `http, _, None -> checksums |> Result.map
		(fun checksums -> `http (url_without_backend, checksums)) (* drop the VCS portion *)
	| `http, _, Some _ -> Error (`unsupported_archive "http with fragment")
	| `rsync, transport, None -> Error (`unsupported_archive ("rsync transport: " ^ transport))
	| `rsync, _, Some _ -> Error (`unsupported_archive "rsync with fragment")

let _post_load_opam ~desc loaded =
	if OPAM.format_errors loaded <> [] then (
		OPAM.print_errors loaded;
		failwith (Printf.sprintf "Invalid OPAM file contents:\n%s" desc)
	);
	loaded |> OpamFormatUpgrade.opam_file

let load_opam_string str =
	_post_load_opam ~desc:("\n" ^ str) (OPAM.read_from_string str)

let load_opam path =
	Util.debug "Loading opam file: %s\n" path;
	if not (Sys.file_exists path) then raise (Invalid_package ("No opam file at " ^ path));
	let open OpamFilename in
	(* TODO could pass this in if we want to embrace OpamFilename more fully *)
	let dir = Dir.of_string (Filename.dirname path) in
	let base = Base.of_string (Filename.basename path) in
	let file = OpamFilename.create dir base |> OpamFile.make in
	_post_load_opam ~desc:path (OPAM.read file)

let nix_of_url ~cache (url:url) : (Nix_expr.t, Digest_cache.error) Result.t Lwt.t =
	let open Nix_expr in
	match url with
		| `http (src, checksums) ->
			Digest_cache.add src checksums cache |> Lwt.map (fun digest ->
				digest |> Result.map (function
					| `sha256 sha256 -> "sha256", str sha256
				) |> Result.map (fun digest ->
					`Call [
						`Lit "pkgs.fetchurl";
						`Attrs (AttrSet.build [ "url", str src; digest ])
					]
				)
			)

let unsafe_drvname_chars = Str.regexp "[^-_.0-9a-zA-Z]"
let drvname_safe str =
	(Str.global_replace unsafe_drvname_chars "-" str)

let add_implicit_build_dependencies ~add_dep commands =
	let implicit_optdeps = ref StringSet.empty in
	(* If your build command depends on foo:installed, you have an implicit optional
	 * build dependency on foo. Packages *should* declare this, but don't always... *)
	let lookup_var key =
		match Vars.implicit_package_var key with
			| None -> None
			| Some pkg ->
				let pkgname = OpamPackage.Name.to_string pkg in
				debug "  adding implied dep: %s\n" pkgname;
				implicit_optdeps := !implicit_optdeps |> StringSet.add pkgname;
				(* value doesn't actually matter, since we don't use the result *)
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

let add_opam_deps ~add_dep (opam:OPAM.t) =
	add_implicit_build_dependencies ~add_dep [OPAM.build opam; OPAM.install opam];
	add_dep Optional (PackageDependencies (OPAM.depopts opam));
	add_dep Required (PackageDependencies (OPAM.depends opam));
	let depexts = OPAM.depexts opam in
	if (depexts <> []) then add_dep Required (ExternalDependencies depexts)
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

type opam_src = [ `Dir of Nix_expr.t | `File of Nix_expr.t ]

let nix_of_opam ~pkg ~deps ~(opam_src:opam_src) ~opam ~src ~url () : Nix_expr.t =
	let name = OpamPackage.name pkg |> OpamPackage.Name.to_string in
	let version = OpamPackage.version pkg |> OpamPackage.Version.to_string in
	let open Nix_expr in
	let adder r = fun importance name -> r := InputMap.add name importance !r in

	deps#init_package pkg;

	let opam_inputs = ref InputMap.empty in
	let nix_deps = ref InputMap.empty in
	let add_native = adder nix_deps in
	let add_opam_input = adder opam_inputs in

	let add_dep = fun importance dep ->
		add_nix_inputs
			~add_native
			~add_opam:add_opam_input
			importance dep
	in

	let () = add_opam_deps ~add_dep opam in

	let url_ends_with ext = (match url with
		| Some (`http (url,_)) -> ends_with ext url
		| None -> false
	) in

	if url_ends_with ".zip" then add_native Required "unzip";

	let property_of_input src (name, importance) : Nix_expr.t =
		match importance with
			| Optional -> `Property_or (src, name, `Null)
			| Required -> `PropertyPath (src, String.split_on_char '.' name)
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

	(* TODO: separate build-only deps from propagated *)
	`Attrs (AttrSet.build ([
		"pname", Nix_expr.str (drvname_safe name);
		"version", Nix_expr.str (drvname_safe version);
		"src", (src |> Option.default `Null);
		"opamInputs", `Attrs opam_inputs;
		"opamSrc", (match opam_src with
			| `Dir expr -> expr
			| `File expr -> expr
		);
	] @ (if nix_deps = [] then [] else ["buildInputs", `List nix_deps])))
