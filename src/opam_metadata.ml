open Util
module URL = OpamFile.URL
module OPAM = OpamFile.OPAM
module Descr = OpamFile.Descr
open OpamTypes

module StringMap = struct
	include Map.Make(String)
	let from_list items = List.fold_right (fun (k,v) map -> add k v map) items empty
end


let var_prefix = "opam_var_"

type dependency =
	| NixDependency of string
	| OcamlDependency of OpamTypes.compiler_constraint
	| OsDependency of (bool * string) OpamTypes.generic_formula
	| ExternalDependencies of OpamTypes.tags
	| PackageDependencies of OpamTypes.ext_formula

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

let rec iter_formula : 'a . (importance -> 'a -> unit) -> importance -> 'a OpamFormula.formula -> unit =
	fun iter_atom importance formula ->
		let recurse = iter_formula iter_atom in
		let open OpamFormula in
		match formula with
			| Empty -> ()
			| Atom a -> iter_atom importance a
			| Block b -> recurse importance b
			| And (a,b) -> recurse importance a; recurse importance b
			| Or(a,b) -> recurse Optional a; recurse Optional b

let string_of_dependency = function
	| NixDependency dep -> "nix:"^dep
	| OcamlDependency _ -> "ocaml:<TODO>"
	| OsDependency formula ->
			"os:" ^
				(OpamFormula.string_of_formula (fun (b,s) -> (string_of_bool b) ^","^s) formula)
	| ExternalDependencies tags ->
			"external:" ^
				(OpamMisc.StringSetMap.to_string (OpamMisc.StringSet.to_string) tags)
	| PackageDependencies formula ->
		(* of OpamTypes.ext_formula *)
		"package:<TODO>"

let string_of_requirement = function
	| Required, dep -> string_of_dependency dep
	| Optional, dep -> "{" ^ (string_of_dependency dep) ^ "}"

let add_nix_inputs
	~(add_native: importance -> string -> unit)
	~(add_opam:importance -> string -> unit)
	importance dep =
	let desc = match importance with
		| Required -> "dep"
		| Optional -> "optional dep"
	in
	(* let depend_on = add_input importance in *)
	match dep with
		| NixDependency name ->
				Printf.eprintf "  adding nix %s: %s\n" desc name;
				add_native importance name
		| OcamlDependency _dep ->
				Printf.eprintf "  adding ocaml %s: %s\n" desc "ocaml";
				add_native importance "ocaml" (* XXX include version *)
		| OsDependency formula ->
				iter_formula (fun importance (b, str) ->
					Printf.eprintf "TODO: OS %s (%b,%s)\n" desc b str
				) importance formula
		| ExternalDependencies externals ->
				let has_nix = ref false in
				let add_all importance packages =
					OpamMisc.StringSet.iter (fun dep ->
						Printf.eprintf "  adding nix %s: %s\n" desc dep;
						add_native importance dep
					) packages
				in

				OpamMisc.StringSetMap.iter (fun environments packages ->
					if OpamMisc.StringSet.mem "nixpkgs" environments then (
						has_nix := true;
						add_all importance packages
					)
				) externals;
				if not !has_nix then begin
					Printf.eprintf
						"  Note: package has depexts, but none of them `nixpkgs`:\n    %s\n"
						(OpamMisc.StringSetMap.to_string (OpamMisc.StringSet.to_string) externals);
					Printf.eprintf "  Adding them all as `optional` deps, with fingers firmly crossed.\n";
					OpamMisc.StringSetMap.iter (fun _environments packages ->
						add_all Optional packages
					) externals;
				end

		| PackageDependencies formula ->
			let iter_dep importance (dep:OpamPackage.Name.t * (package_dep_flag list * OpamFormula.version_formula)) =
				let (name, (flags, version_formula)) = dep in
				let name = OpamPackage.Name.to_string name in
				let needed_for_build = if List.length flags = 0
					then true
					else flags |> List.fold_left (fun required flag ->
						required || (match flag with Depflag_Build -> true | _ -> false)) false
				in
				if needed_for_build then begin
					let version_desc = ref "" in
					version_formula |> iter_formula (fun _importance (relop, version) ->
						version_desc := !version_desc ^ (
							(OpamFormula.string_of_relop relop) ^
							(OpamPackage.Version.to_string version)
						)
					) importance;
					Printf.eprintf "  adding %s: %s%s\n" desc name !version_desc;
					add_opam importance name
				end else
					Printf.eprintf "  skipping non-build %s on %s\n" desc name
			in
			iter_formula iter_dep importance formula

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


type url = [
	| `http of string
	| `git of string * string option
]

let url file = match (URL.kind file, URL.url file) with
	| `http, (src, None) -> `http src
	| `http, (src, Some what) -> failwith "http url with fragment"
	| `git, src -> `git src
	| `local, _ -> failwith "`local url not supported"
	| `darcs, _ -> failwith "TODO: darcs"
	| `hg, _ -> failwith "TODO: hg"

let load_url path =
	if Sys.file_exists path then begin
		let url_file = open_in path in
		let rv = URL.read_from_channel url_file in
		close_in url_file;
		Some (url rv)
	end else None

let load_opam path =
	Printf.eprintf "  Loading opam info from %s\n" path;
	let file = open_in path in
	let rv = OPAM.read_from_channel file in
	close_in file;
	rv

let concat_address (addr, frag) =
	match frag with
		| Some frag -> addr ^ "#" ^ frag
		| None -> addr

let string_of_url url =
	match url with
		| `http addr -> addr
		| `git addr -> "git:" ^ (concat_address addr)

let sha256_of_path p =
	let open Lwt in
	lwt lines = Lwt_process.with_process_in ("", [|"nix-hash"; "--base32"; "--flat"; "--type";"sha256"; p|]) (fun proc ->
		lwt lines = proc#stdout |> Lwt_io.read_lines |> Lwt_stream.to_list in
		lwt status = proc#close in
		let open Unix in
		match status with
			| WEXITED 0 -> return lines
			| _ -> failwith "nix-hash failed"
	) in
	match lines with
		| [line] -> return line
		| _ -> failwith ("Unexpected nix-hash output:\n" ^ (String.concat "\n" lines))

let nix_of_url ~add_input ~cache (url:url) =
	let open Nix_expr in
	let local_copy = cache#download url in
	let sha256 = sha256_of_path local_copy |> Lwt_main.run in
	match url with
		| `http src ->
			add_input "fetchurl";
			`Call [
				`Id "fetchurl";
				`Attrs (AttrSet.build [
					"url", str src;
					"sha256", str sha256;
				]);
			]
		| `git addr ->
				add_input "fetchgit";
				`Call [
					`Id "fetchurl";
					`Attrs (AttrSet.build [
						"url", str (concat_address addr);
						"sha256", str sha256;
					]);
				]

let unsafe_envvar_chars = Str.regexp "[^0-9a-zA-Z_]"
let envvar_of_ident name =
	var_prefix ^ (Str.global_replace unsafe_envvar_chars "_" name)

let attrs_of_opam ~add_dep (opam:OPAM.t) =
	add_dep Required (match (OPAM.ocaml_version opam) with
		| None -> NixDependency "ocaml"
		| Some constr -> OcamlDependency constr
	);
	add_dep Optional (PackageDependencies (OPAM.depopts opam));
	add_dep Required (PackageDependencies (OPAM.depends opam));
	add_dep Required (OsDependency (OPAM.os opam));
	let () = match OPAM.depexts opam with
		| None -> ()
		| Some deps -> add_dep Required (ExternalDependencies deps);
	in

	[
		"configurePhase", Nix_expr.str "true"; (* configuration is done in build commands *)
		"buildPhase", `Lit "\"${opam2nix}/bin/opam2nix invoke build\"";
		"installPhase", `Lit "\"${opam2nix}/bin/opam2nix invoke install\"";
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

let nix_of_opam ~name ~version ~cache ~deps ~has_files path : Nix_expr.t =
	let pkgid = OpamPackage.create
		(OpamPackage.Name.of_string name)
		(OpamPackage.Version.of_string version)
	in
	let open Nix_expr in
	let inputs = ref (InputMap.from_list [
		"stdenv", Required;
		"pkgs", Required;
		"opamSelection", Required;
		"opam2nix", Required;
	]) in
	let additional_env_vars = ref [] in
	let adder r = fun importance name -> r := InputMap.add name importance !r in
	let add_input = adder inputs in

	let url = load_url (Filename.concat path "url") in
	let src = Option.map (nix_of_url ~add_input:(add_input Required) ~cache) url in

	deps#init_package pkgid;

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

	let opam = load_opam (Filename.concat path "opam") in
	let buildAttrs : (string * Nix_expr.t) list = attrs_of_opam ~add_dep opam in

	let opam_inputs : Nix_expr.t AttrSet.t = !opam_inputs |> InputMap.mapi (fun name importance ->
		match importance with
			| Optional -> `Property_or (`Id "opamSelection", name, `Null)
			| Required -> `Property (`Id "opamSelection", name)
	) in

	(* let swap (a, b) = (b, a) in *)
	let inputs = !inputs |> InputMap.bindings in
	let nix_deps = !nix_deps |> InputMap.bindings in

	let input_args = (inputs @ nix_deps)
		|> List.sort (fun (a,_) (b,_) -> String.compare a b)
		|> List.map (function
			| name, Required -> `Id name
			| name, Optional -> `Default (name, `Null)
		) in

	`Let_bindings (
		AttrSet.build [
			"identity", `Lit "x: x";
			"buildWithOverride", `Function (
				`Id "override",
				`Function (
					(`NamedArguments input_args),
					(`Let_bindings (
						(AttrSet.build [
							"lib", `Lit "pkgs.lib";
							"opamDeps", `Attrs opam_inputs;
						]),
						`Call [ `Id "stdenv.mkDerivation";
							`Call [ 
								`Id "override";
								(`Attrs (AttrSet.build (!additional_env_vars @ [
									"name", Nix_expr.str (name ^ "-" ^ version);
									"opamEnv", `Call [`Id "builtins.toJSON"; `Attrs (AttrSet.build [
										"spec", `Lit "./opam";
										"deps", `Lit "opamDeps";
										"files", if has_files then `Lit "./files" else `Null;
									])];
									"buildInputs", `Call [
										`Id "lib.remove";
										`Null;
										`BinaryOp (
											`List (
												(nix_deps |> List.map (fun (name, _importance) -> `Id name))
											),
											"++",
											`Lit "(lib.attrValues opamDeps)"
										);
									];
									"createFindlibDestdir", `Lit "true";
									"passthru", `Attrs (AttrSet.build [
										"opamSelection", `Id "opamSelection";
										"ocaml", `Id "ocaml";
									]);
								] @ (
									match src with
										| Some src -> buildAttrs @ ["src", src]
										| None -> let open Nix_expr in [
											(* psuedo-package. We need it to exist in `opamSelection`, but
											* it doesn't really do anything *)
											"unpackPhase", str "true";
											"buildPhase", str "true";
											"installPhase", str "touch $out";
										]
								))))
							]
						]
					))
				)
			);
			"wrap", `Function (`Id "buildWithOverride", `Attrs (AttrSet.build [
				"impl", `Call [`Id "buildWithOverride"; `Id "identity"];
				"withOverride", `Function (`Id "override",
					`Call [`Id "wrap";
						`Function (`Id "additionalOverride",
							`Call [`Id "buildWithOverride";
								`Function (`Id "attrs",
									`Call [`Id "additionalOverride"; `Call [`Id "override"; `Id "attrs"]]
								)
							]
						)
					]
				);
			]));
				(* "buildWithOverride: { *)
				(* 	impl = buildWithOverride identity; *)
				(* 	withOverride = override: wrap ( *)
				(* 		additionalOverride: (buildWithOverride (attrs: additionalOverride (override attrs))) *)
				(* 	); *)
				(* }"; *)
		],
		`Call [`Id "wrap"; `Id "buildWithOverride"]
	)


let os_string () = OpamGlobals.os_string ()

let add_var name v vars =
	vars |> OpamVariable.Full.Map.add (OpamVariable.Full.of_string name) v

let init_variables () =
	let state = OpamVariable.Full.Map.empty in
	state
		|> add_var "os" (S (os_string ()))
		|> add_var "make" (S "make")
		|> add_var "opam-version" (S (OpamVersion.to_string OpamVersion.current))
		|> add_var "preinstalled" (B false) (* XXX ? *)
		|> add_var "jobs" (S "1") (* XXX NIX_JOBS? *)
		|> add_var "opam-version" (S (OpamVersion.to_string OpamVersion.current))
		(* XXX best guesses... *)
		|> add_var "ocaml-native" (B true)
		|> add_var "ocaml-native-tools" (B true)
		|> add_var "ocaml-native-dynlink" (B true)
		|> add_var "arch" (S (OpamGlobals.arch ()))

let lookup_var vars key =
	try Some (OpamVariable.Full.Map.find key vars)
	with Not_found -> begin
		prerr_endline ("WARN: opam var " ^ (OpamVariable.Full.to_string key) ^ " not found...");
		None
	end
