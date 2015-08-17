open Util
open OpamTypes
module Name = OpamPackage.Name
module Version = OpamPackage.Version

(* consistent_* functions adapted from opamState.ml implementation *)
let consistent_ocaml_version ~ocaml_version opam =
	let atom (r,v) =
		OpamCompiler.Version.eval_relop r (ocaml_version) v
	in
	match OpamFile.OPAM.ocaml_version opam with
	| None -> true
	| Some constr -> OpamFormula.eval atom constr

let consistent_os ~target_os opam =
	match OpamFile.OPAM.os opam with
	| Empty -> true
	| f ->
		let atom (b, os) =
			let ($) = if b then (=) else (<>) in
			os $ target_os in
		OpamFormula.eval atom f


let resolve_variable ~ocaml_version ~global_vars v =
	let string s = Some (S s)
	and bool b = Some (B b) in

	let lookup = Opam_metadata.lookup_var global_vars in

	let get_env_var v =
		let var_str =
			OpamVariable.Full.to_string v
			|> OpamMisc.string_map (function '-' -> '_' | c -> c) in
		try match OpamMisc.getenv ("OPAMVAR_" ^ var_str) with
			| "true"  | "1" -> bool true
			| "false" | "0" -> bool false
			| s             -> string s
		with Not_found -> None
	in

	let get_global_var v =
		let ocaml_version = OpamCompiler.Version.to_string ocaml_version in
		match OpamVariable.Full.to_string v with
		| "ocaml-version" -> string ocaml_version
		| "compiler" -> string ocaml_version (* XXX does this differ from version? *)
		| _ -> None
	in
	let contents =
		try
			List.fold_left
				(function None -> (fun (f,v) -> f v) | r -> (fun _ -> r))
				None
				[
					get_env_var, v;
					(* read_var, v; *)
					get_global_var, v;
					lookup, v;
					(* get_package_var, v'; *)
				]
		with Exit -> None
	in
	contents


let consistent_available_field ~ocaml_version ~vars opam =
	OpamFilter.eval_to_bool ~default:false (resolve_variable ~ocaml_version ~global_vars:vars)
		(OpamFile.OPAM.available opam)


let build_universe ~repo ~packages ~ocaml_version ~base_packages ~target_os () =
	let empty = OpamPackage.Set.empty in
	let names n = Name.Set.of_list (n |> List.map Name.of_string) in
	let available_packages = ref empty in
	let opams = ref OpamPackage.Map.empty in

	let opam_vars = Opam_metadata.init_variables () in

	let availability opam =
		let ocaml_version = OpamCompiler.Version.of_string ocaml_version in
		let rec check = function
			| (fn, rejection) :: remaining ->
					if fn opam
						then check remaining
						else `Unavailable rejection
			| [] -> `Available
		in
		check [
			consistent_ocaml_version ~ocaml_version, "OCaml version " ^ (OpamCompiler.Version.to_string ocaml_version);
			consistent_os ~target_os, "OS " ^ (Opam_metadata.os_string ());
			consistent_available_field ~ocaml_version ~vars:opam_vars, "`available` constraints";
		]
	in

	Repo.traverse `Nix ~repo ~packages:[] (fun package version path ->
		let opam = Opam_metadata.load_opam (Filename.concat path "opam") in
		match availability opam with
			| `Available ->
				let pkg = OpamPackage.create (Name.of_string package) (Version.of_string version) in
				available_packages := OpamPackage.Set.add pkg !available_packages;
				opams := OpamPackage.Map.add pkg opam !opams
			| `Unavailable reason ->
				Printf.eprintf "  # Ignoring package %s-%s (incomatible with %s)\n" package version reason
	);
	let opams = !opams in
	let ocaml_version = Version.of_string ocaml_version in
	{
		u_packages	= empty;
		u_action		= Install (names packages); (* XXX this duplicates "atom request" below *)
		u_installed = empty;
		u_available = !available_packages;
		u_depends		= OpamPackage.Map.map OpamFile.OPAM.depends opams;
		u_depopts		= OpamPackage.Map.map OpamFile.OPAM.depopts opams;
		u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
		u_installed_roots = empty;
		u_pinned		= empty;
		(* u_dev			 = empty; *)
		u_base			= base_packages
			|> List.map (fun name -> OpamPackage.create (Name.of_string name) ocaml_version)
			|> OpamPackage.Set.of_list;
		(* u_attrs		 = []; *)
		(* u_test			 = false; *)
		(* u_doc			 = false; *)
	}

let main idx args =
	let repo = ref "" in
	let dest = ref "" in
	(* XXX this should be more integrated... *)
	let ocaml_version = ref "" in
	let ocaml_attr = ref "ocaml" in
	let base_packages = ref "" in
	let target_os = ref (Opam_metadata.os_string ()) in
	let opts = Arg.align [
		("--repo", Arg.Set_string repo, "Repository root");
		("--dest", Arg.Set_string dest, "Destination .nix file");
		("--os", Arg.Set_string target_os, "Target OS");
		("--ocaml-version", Arg.Set_string ocaml_version, "Target ocaml version");
		("--ocaml-attr", Arg.Set_string ocaml_attr, "Ocaml nixpkgs attribute (e.g `ocaml`, `ocaml_4_00_01`)");
		("--base-packages", Arg.Set_string base_packages, "Available base packages (comma-separated)");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse_argv ~current:(ref idx) args opts add_package "TODO: usage...";

	if !packages = [] then failwith "At least one package required";
	let packages = !packages in
	let dest = nonempty !dest "--dest" in
	let repo = nonempty !repo "--repo" in
	let package_names = packages |> List.map OpamPackage.Name.of_string in

	let ocaml_version = nonempty !ocaml_version "--ocaml-version" in
	let ocaml_attr = !ocaml_attr in
	let base_packages = nonempty !base_packages "--base-packages" |> Str.split (Str.regexp ",") in

	let universe = build_universe
		~repo:repo
		~packages:packages
		~base_packages
		~ocaml_version
		~target_os:!target_os
		() in
	let request = {
		wish_install = package_names |> List.map (fun name -> name, None); (* XXX version *)
		wish_remove = [];
		wish_upgrade = [];
		criteria = `Default;
	} in
	let () = match OpamSolver.resolve ~verbose:true universe ~orphans:OpamPackage.Set.empty request with
		| Success solution ->
				prerr_endline "Solved!";
				OpamSolver.print_solution
					~messages:(fun pkg -> [OpamPackage.to_string pkg])
					~rewrite:(fun x -> x)
					~requested:(package_names |> OpamPackage.Name.Set.of_list)
					solution;
				let open Nix_expr in
				let selection = OpamPackage.Set.fold (fun pkg map ->
					print_endline ("install: " ^ (OpamPackage.to_string pkg));
					AttrSet.add (OpamPackage.name pkg |> Name.to_string)
						(`Call [
							`Id "builder";
							`Id "selection";
							`PropertyPath (`Id "opamPackages", [
								pkg |> OpamPackage.name |> Name.to_string;
								pkg |> OpamPackage.version |> Version.to_string;
							]);
						])
						map
				) (OpamSolver.new_packages solution) AttrSet.empty in
				let selection = AttrSet.add "ocaml" (`Id "ocaml") selection in
				let selection = List.fold_right (fun base -> AttrSet.add base (`Lit "true")) base_packages selection in

				let expr = (`Function (
					`NamedArguments [
						`Id "pkgs";
						`Id "opam2nix";
						`Id "opamPackages";
						`Default ("ocaml", `Property (`Id "pkgs", ocaml_attr));
						`Default ("builder", `Lit "opamSelection: pkg: pkgs.callPackage pkg.impl { inherit opamSelection opam2nix; }");
						`Default ("overrideSelections", `Lit "sels: sels");
					],
					`Let_bindings (
						AttrSet.build ([
							"selection", `Call [ `Id "overrideSelections"; `Attrs selection ];
						]),
						`Id "selection"
					)
				)) in
				let oc = open_out dest in
				Nix_expr.write oc expr;
				close_out oc
		| Conflicts conflict ->
			prerr_endline (
				OpamCudf.string_of_conflict
					(fun p -> "package " ^ (OpamFormula.string_of_atom p) ^ " unavailable")
					conflict
			);
			exit 1
	in
	()
