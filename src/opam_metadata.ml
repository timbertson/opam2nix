open Util
module URL = OpamFile.URL
module OPAM = OpamFile.OPAM
module Descr = OpamFile.Descr
open OpamTypes
module StringMap = Map.Make(String)

let var_prefix = "opam_var_"

type dependency =
	| NixDependency of string
	| OcamlDependency of OpamTypes.compiler_constraint
	| OsDependency of (bool * string) OpamTypes.generic_formula
	| ExternalDependencies of OpamTypes.tags
	| PackageDependencies of OpamTypes.ext_formula

type importance = Required | Optional
type requirement = importance * dependency

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
				add_native importance "ocaml" (* XXX ocaml version *)
		| OsDependency _dep ->
				Printf.eprintf "  adding OS %s: <TODO>\n" desc;
				Printf.eprintf "TODO: OsDependency\n"
		| ExternalDependencies externals ->
				let has_nix = ref false in
				OpamMisc.StringSetMap.iter (fun environments packages ->
					if OpamMisc.StringSet.mem "nixpkgs" environments then (
						has_nix := true;
						OpamMisc.StringSet.iter (fun dep ->
							Printf.eprintf "  adding nix %s: %s\n" desc dep;
							add_native importance dep
						) packages
					)
				) externals;
				if not !has_nix then
					Printf.eprintf
						"Note: package has depexts, but none of them `nixpkgs`:\n%s\n"
						(OpamMisc.StringSetMap.to_string (OpamMisc.StringSet.to_string) externals)
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

(* type opam_package_id = { *)
(* 	opam_name: string; *)
(* 	opam_version: OpamTypes.version; *)
(* } *)
(*  *)
(* module OpamPackageOrd = struct *)
(* 	type t = opam_package_id *)
(* 	let compare a b = *)
(* 		let name_diff = String.compare a.opam_name b.opam_name in *)
(* 		if name_diff <> 0 *)
(* 			then name_diff *)
(* 			else OpamPackage.Version.compare (a.opam_version) (b.opam_version) *)
(* end *)

module PackageMap = OpamPackage.Map

(* module Deps = struct *)
(* 	type t = dependency list PackageMap.t *)
(* 	let add t package_id dep = *)
(* 		let existing = try PackageMap.find package_id with Not_found -> [] in *)
(* 		PackageMap.add package_id (dep :: existing) t *)
(* end *)

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
	let url_file = open_in path in
	let rv = URL.read_from_channel url_file in
	close_in url_file;
	url rv

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
	add_dep Required (PackageDependencies (OPAM.depends opam));
	add_dep Optional (PackageDependencies (OPAM.depopts opam));
	add_dep Required (OsDependency (OPAM.os opam));

	(* let mkCommands steps = `MultilineString (steps *)
	(* 	|> List.map (fun step -> [step; `Lit "\n"]) *)
	(* 	|> List.concat) *)
	(* in *)

	(* let mkCommands2 steps = `MultilineString (steps *)
	(* 	|> List.map (fun args -> `Lit ((String.concat " " args) ^ "\n"))) *)
	(* in *)

	(* let rec evaluate_filter = function *)
	(* 	| FBool b -> "bool " ^ (string_of_bool b) *)
	(* 	| FString s -> s *)
	(* 	| FIdent (packages,var, -> "$" ^ (envvar_of_ident s) *)
	(* 	| FNot f -> "!" ^ (evaluate_filter f) *)
	(* 	| FOp (a, op, b) -> *)
	(* 		let a = evaluate_filter a *)
	(* 		and b = evaluate_filter b in *)
	(* 		(match op with *)
	(* 			| `Eq -> a^"=="^b *)
	(* 			| `Neq -> a^"!="^b *)
	(* 			| `Geq -> a^">="^b *)
	(* 			| `Gt -> a^">"^b *)
	(* 			| `Leq -> a^"<="^b *)
	(* 			| `Lt -> a^"<"^b *)
	(* 		) *)
	(* 	| FAnd (a,b) -> (evaluate_filter a) ^ "&&" ^ (evaluate_filter b) *)
	(* 	| FOr (a,b) -> (evaluate_filter a) ^ "||" ^ (evaluate_filter b) *)
	(* in *)

	(* let env s = *)
	(* 	try Some (OpamVariable.Full.Map.find s state) *)
	(* 	with Not_found -> None *)
	(* in *)
	(* let string_of_arg (arg, filter) = *)
	(* 	let argstr = match arg with *)
	(* 		(* XXX do we need to bash-escape these? *) *)
	(* 		| CString s -> s *)
	(* 		| CIdent s -> "\"$" ^ (envvar_of_ident s) ^ "\"" *)
	(* 	in *)
	(* 	(match filter with *)
	(* 		| Some filter -> "("^argstr ^ ")["^(evaluate_filter filter)^"]" *)
	(* 		| None -> argstr) *)
	(* in *)
	(* let string_of_command (args, filter) = *)
	(* 	let cmd = args |> List.map string_of_arg |> String.concat " " in *)
	(* 	`Lit (match filter with *)
	(* 		| Some filter -> *)
	(* 				let filtered = OpamFilter.eval env filter in *)
	(* 				"("^cmd ^ ")["^(evaluate_filter filter)^"->"^(string_of_bool filtered)^"]" *)
	(* 		| None -> cmd) *)
	(* in *)
	(* let string_of_patch (basename, filter) = *)
	(* 	(* patches live in `files`, which is copied into source root. *)
	(* 	 * So we can just use strings, not path references *) *)
	(* 	let path = Nix_expr.str (OpamFilename.Base.to_string basename) in *)
	(* 	match filter with *)
	(* 		| None -> Some path *)
	(* 		| Some filter -> (match OpamFilter.eval env filter with *)
	(* 			| S str -> failwith ("Expected `patch` filter to return a bool; got " ^ str) *)
	(* 			| B true -> Some path *)
	(* 			| B false -> None *)
	(* 		) *)
	(* in *)
	[
		(* "preConfigure", mkCommands (List.concat [ *)
		(* 	(OPAM.substs opam |> List.map (fun subst -> failwith "TODO: OPAM.substs")); *)
		(* 	(OPAM.build_env opam |> List.map (fun env -> failwith "TODO: build_env")); *)
		(* ]); *)
		"configurePhase", Nix_expr.str "true"; (* configuration is done in build commands *)
		"buildPhase", `Lit "\"${opam2nix}/bin/_opam2nix_invoke build\"";
		"installPhase", `Lit "\"${opam2nix}/bin/_opam2nix_invoke install\"";
		(* mkCommands2 ( *)
		(* 	let () = OPAM.build opam |> OpamFilter.commands_variables |> List.map OpamVariable.Full.to_string |> String.concat " " |> Printf.eprintf "XXX vars: %s\n" in *)
		(* 	(OPAM.build opam |> OpamFilter.commands env) *)
		(* ); *)
		(* "installCommand", mkCommands (List.concat [ *)
		(* 	(OPAM.install opam |> List.map (string_of_command)) *)
		(* ]); *)
		(* "prePatch", Nix_expr.str "export patches \"$(opam2nix-invoke print-patches)\""; *)
		(* "patches", `List (OPAM.patches opam |> filter_map (string_of_patch)); *)
	]
	(* @ ( *)
	(* 	match files with [] -> [] | files -> [ *)
	(* 		"prePatch", `String ( *)
	(* 			[`Lit "cp  "] *)
	(* 			@ (files |> List.map (fun filename -> *)
	(* 				[ `Expr (`Lit ("./files/" ^ filename)); `Lit " " ] *)
	(* 			) |> List.concat) *)
	(* 			@ [ `Lit "./"] *)
	(* 		) *)
	(* 	] *)
	(* ) *)
;;



(* let info_opam ic = *)
(* 	`O [ *)
(* 		("type", `String "opam"); *)
(* 		("ocaml_version", match (OPAM.ocaml_version file) with *)
(* 			| None -> `Null *)
(* 			| Some constr -> json_of_formula json_of_compiler_version_constraint constr *)
(* 		); *)
(* 		("os", json_of_formula json_of_os_constr (OPAM.os file)); *)
(* 		("depends", json_of_depends (OPAM.depends file)); *)
(* 		("depends_optional", json_of_depends (OPAM.depopts file)); *)
(* 		("depends_external", match OPAM.depexts file with *)
(* 			| None -> `Null *)
(* 			| Some deps -> *)
(* 				OpamMisc.StringSetMap.to_json OpamMisc.StringSet.to_json deps; *)
(* 		); *)
(* 		("conflicts", json_of_depends (OPAM.conflicts file)); *)
(* 	] *)


let nix_of_opam ~name ~version ~cache ~deps ~has_files path : Nix_expr.t =
	let pkgid = OpamPackage.create
		(OpamPackage.Name.of_string name)
		(OpamPackage.Version.of_string version)
	in
	let open Nix_expr in
	let inputs = ref [ Required, "stdenv"; Required, "pkgs"; Required, "opamSelection"; Required, "opam2nix" ] in
	let additional_env_vars = ref [] in
	let adder r = fun importance name -> r := (importance, name) :: !r in
	let add_input = adder inputs in

	let url = load_url (Filename.concat path "url") in
	let src = nix_of_url ~add_input:(add_input Required) ~cache url in

	deps#init_package pkgid;

	let opam_inputs = ref [ ] in
	let nix_deps = ref [] in
	let add_native = adder nix_deps in
	let add_opam_input importance name =
		(* let envvar = envvar_of_ident ("enabled_"^name) in *)
		(* state := OpamVariable.Full.Map.add *)
		(* 	(OpamVariable.Full.of_string (name ^ ":enabled")) *)
		(* 	(S ("$" ^ envvar)) *)
		(* 	!state; *)
		(* additional_env_vars := (envvar, (`BinaryOp (`Id name, "!=", `Lit "null"))) :: !additional_env_vars; *)
		opam_inputs := (importance, name) :: !opam_inputs
	in
	let add_dep = fun importance dep ->
		(* deps#add_dep pkgid dep; *)
		add_nix_inputs
			~add_native
			~add_opam:add_opam_input
			importance dep
	in

	let opam = load_opam (Filename.concat path "opam") in
	let buildAttrs : (string * Nix_expr.t) list = attrs_of_opam ~add_dep opam in

	(* clean up potentially-duplicate `inputs` *)
	let opam_inputs =
		let rv = ref AttrSet.empty in
		let mandatory, optional = !opam_inputs |> List.partition (fun (i,_) -> i = Required) in
		let snd = fun (a,b) -> b in
		optional |> List.map snd |> List.iter (fun name ->
			rv := AttrSet.add name (`Property_or (`Id "opamSelection", name, `Null)) !rv
		);
		mandatory |> List.map snd |> List.iter (fun name ->
			rv := AttrSet.add name (`Property (`Id "opamSelection", name)) !rv
		);
		rv
	in

	let inputs =
		let mandatory, optional = !inputs |> List.partition (fun (i,_) -> i = Required) in
		let snd = fun (a,b) -> b in
		let mandatory = mandatory |> List.map snd in
		let optional = optional |> List.map snd |> List.filter (fun name ->
			not (List.mem name mandatory)
		) in
		let export = fun importance items ->
			items |> List.sort compare |> List.map (fun name -> (importance, name))
		in
		(export Required mandatory @ export Optional optional)
	in

	let nix_deps =
		let mandatory, optional = !nix_deps |> List.partition (fun (i,_) -> i = Required) in
		let snd = fun (a,b) -> b in
		let mandatory = mandatory |> List.map snd in
		let optional = optional |> List.map snd |> List.filter (fun name ->
			not (List.mem name mandatory)
		) in
		let export = fun importance items ->
			items |> List.sort compare |> List.map (fun name -> (importance, name))
		in
		(export Required mandatory @ export Optional optional)
	in

	let input_args = (inputs @ nix_deps) |> List.map (function
		| Required, name -> `Id name
		| Optional, name -> `Default (name, `Null)
	) in

	(`Function (
		(`NamedArguments input_args),
		(`Let_bindings (
			(AttrSet.build [
				"lib", `Lit "pkgs.lib";
				"opamDeps", `Attrs !opam_inputs;
			]),
			(`Call [
				`Id "stdenv.mkDerivation";
				(`Attrs (AttrSet.build (buildAttrs @ !additional_env_vars @ [
					"name", Nix_expr.str (name ^ "-" ^ version);
					"src", src;
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
								(nix_deps |> List.map (fun (dep, name) -> `Id name))
							),
							"++",
							`Lit "(lib.attrValues opamDeps)"
						);
					];
					"createFindlibDestdir", `Lit "true";
				])))
			])
		))
	))


