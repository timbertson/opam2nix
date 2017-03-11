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
			|> OpamStd.String.map (function '-' -> '_' | c -> c) in
		try match OpamStd.Env.get ("OPAMVAR_" ^ var_str) with
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

let print_universe chan u =
	match u with { u_action; u_available; u_installed; _ } -> begin
		let open Printf in
		let string_of_packageset = OpamPackage.Set.to_string in
		let string_of_name_set = OpamPackage.Name.Set.to_string in
		let string_of_action = function
			| Depends -> "Depends"
			| Remove  -> "Remove"
			| Init    -> "Init"
			| Install names -> "Install: " ^ (string_of_name_set names)
			| Switch  names -> "Switch: "  ^ (string_of_name_set names)
			| Import  names -> "Import: "  ^ (string_of_name_set names)
			| Upgrade   packages -> "Upgrade: "   ^ (string_of_packageset packages)
			| Reinstall packages -> "Reinstall: " ^ (string_of_packageset packages)
		in
		fprintf chan "Available:\n";
		u_available |> OpamPackage.Set.iter (fun pkg -> fprintf chan " - %s\n" (OpamPackage.to_string pkg));
		fprintf chan "Installed:\n";
		u_installed |> OpamPackage.Set.iter (fun pkg -> fprintf chan " - %s\n" (OpamPackage.to_string pkg));
		fprintf chan "Action: %s\n" (string_of_action u_action);
		()
	end

let build_universe ~repos ~package_names ~ocaml_version ~base_packages ~target_os () =
	let empty = OpamPackage.Set.empty in
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

	Repo.traverse `Nix ~repos ~packages:[`All] (fun package version path ->
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
	let base_packages = base_packages
		|> List.map (fun name -> OpamPackage.create (Name.of_string name) ocaml_version)
		|> OpamPackage.Set.of_list in
	{ OpamSolver.empty_universe with
		u_action    = Install (OpamPackage.Name.Set.of_list package_names);
		u_installed = base_packages;
		u_base      = base_packages;
		u_available = !available_packages;
		u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
		u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
		u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
	}

let newer_versions available pkg =
	let newer a b =
		(* is a newer than b? *)
		OpamPackage.Version.compare (OpamPackage.version a) (OpamPackage.version b) > 0
	in
	OpamPackage.Set.filter (fun avail ->
		OpamPackage.name avail == OpamPackage.name pkg && newer avail pkg
	) available
	|> OpamPackage.Set.elements
	|> List.sort (fun a b ->
		OpamPackage.Version.compare (OpamPackage.version a) (OpamPackage.version b)
	)

let main idx args =
	let repos = ref [] in
	let dest = ref "" in
	(* XXX this should be more integrated... *)
	let ocaml_version = ref "" in
	let ocaml_attr = ref "ocaml" in
	let base_packages = ref "" in
	let target_os = ref (Opam_metadata.os_string ()) in
	let verbose = ref false in
	let opts = Arg.align [
		("--repo", Arg.String (fun repo -> repos := repo :: !repos), "Repository root");
		("--dest", Arg.Set_string dest, "Destination .nix file");
		("--os", Arg.Set_string target_os, "Target OS");
		("--ocaml-version", Arg.Set_string ocaml_version, "Target ocaml version");
		("--ocaml-attr", Arg.Set_string ocaml_attr, "Ocaml nixpkgs attribute (e.g `ocaml`, `ocaml_4_00_01`)");
		("--base-packages", Arg.Set_string base_packages, "Available base packages (comma-separated)");
		("--verbose", Arg.Set verbose, "Verbose");
		("-v", Arg.Set verbose, "Verbose");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse_argv ~current:(ref idx) args opts add_package "opam2nix: usage...";

	if !packages = [] then failwith "At least one package required";
	let packages = !packages in
	let dest = nonempty !dest "--dest" in
	let repos = nonempty_list !repos "--repo" in

	let () =
		(* make sure opam uses external solver - internal solver is prone to picking old versions *)
		let open OpamTypes in
		OpamSolverConfig.update
			~external_solver:(lazy (Some ([CIdent "aspcud", None])))
			(* cribbed from https://github.com/ocaml/opam/blob/84d01df940297963c82936b0d0de3722f479b56c/src/solver/opamSolverConfig.ml *)
			~solver_preferences_default:(Some (lazy "-removed,-notuptodate,-changed"))
			();
		if not (OpamCudf.external_solver_available ()) then
			failwith "External solver (aspcud) not available";
		if !verbose then
			OpamCoreConfig.update ~debug_level:2 ()
	in

	let requested_packages : OpamFormula.atom list = packages |> List.map (fun spec ->
		let relop_re = Str.regexp "[!<=>]+" in
		match Str.full_split relop_re spec with
			| [Str.Text name; Str.Delim relop; Str.Text ver] ->
				let relop = OpamFormula.relop_of_string relop in
				let ver = OpamPackage.Version.of_string ver in
				(OpamPackage.Name.of_string name, Some (relop, ver))
			| [Str.Text name] -> (OpamPackage.Name.of_string name, None)
			| _ -> failwith ("Invalid version spec: " ^ spec)
	) in
	let package_names : OpamPackage.Name.t list = requested_packages |> List.map (fun (name, _) -> name) in

	let ocaml_version = nonempty !ocaml_version "--ocaml-version" in
	let ocaml_attr = !ocaml_attr in
	let base_packages = nonempty !base_packages "--base-packages" |> Str.split (Str.regexp ",") in

	let universe = build_universe
		~repos:repos
		~package_names
		~base_packages
		~ocaml_version
		~target_os:!target_os
		() in
	if !verbose then print_universe stderr universe;
	(* let request = OpamSolver.request ~install:requested_packages in *)
	let request = {
		wish_install = requested_packages;
		wish_remove = [];
		wish_upgrade = [];
		extra_attributes = [];
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
				let new_packages = OpamSolver.new_packages solution in
				let () = match OpamPackage.Set.fold (fun pkg lst ->
					lst @ (newer_versions universe.u_available pkg)
				) new_packages [] with
					| [] -> ()
					| newer_versions ->
						Printf.eprintf "\nNOTE:\nThe following package versions are newer than the selected versions,\nbut were not selected due to version constraints:\n";
						newer_versions |> List.iter (fun pkg ->
							Printf.eprintf " - %s\n" (OpamPackage.to_string pkg)
						)
				in

				let selection = OpamPackage.Set.fold (fun pkg map ->
					AttrSet.add (OpamPackage.name pkg |> Name.to_string)
						(`PropertyPath (`Id "opamPackages", [
							pkg |> OpamPackage.name |> Name.to_string;
							pkg |> OpamPackage.version |> Version.to_string;
						]))
						map
				) new_packages AttrSet.empty in
				let selection = AttrSet.add "ocaml" (`Call [
					`Id "world.overrideOcaml";
					`Property (`Id "world.pkgs", ocaml_attr)
				]) selection in
				let selection = List.fold_right (fun base -> AttrSet.add base (`Lit "true")) base_packages selection in

				let expr = `Function (
					`Id "world",
					`Let_bindings (AttrSet.build [
						"opamPackages", `Property (`Id "world", "opamPackages");
					], `Attrs selection);
				) in
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
