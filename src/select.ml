open Util
open OpamTypes
module Name = OpamPackage.Name
module Version = OpamPackage.Version

let print_universe chan u =
	match u with { u_available; u_installed; _ } -> begin
		let open Printf in
		let print_package_set = OpamPackage.Set.iter (fun pkg -> fprintf chan " - %s\n" (OpamPackage.to_string pkg)) in
		fprintf chan "Available:\n";
		u_available |> print_package_set;
		fprintf chan "Installed:\n";
		u_installed |> print_package_set;
		()
	end

let build_universe ~repos ~ocaml_version ~base_packages () =
	let empty = OpamPackage.Set.empty in
	let available_packages = ref empty in
	let opams = ref OpamPackage.Map.empty in

	let env = Opam_metadata.init_variables () |> Opam_metadata.lookup_var in

	Repo.traverse `Nix ~repos ~packages:[`All] (fun package version path ->
		let opam = Opam_metadata.load_opam (Filename.concat path "opam") in
		let available_filter = OpamFile.OPAM.available opam in
		let available =
			try package <> "opam" && OpamFilter.eval_to_bool env available_filter
			with e -> (
				Printf.eprintf "Assuming package %s is unavailable due to error: %s\n" package (Printexc.to_string e);
				false
			)
		in
		if available then (
			let pkg = OpamPackage.create (Name.of_string package) (Repo.opam_version_of version) in
			available_packages := OpamPackage.Set.add pkg !available_packages;
			opams := OpamPackage.Map.add pkg opam !opams
		) else (
			let vars = OpamFilter.variables available_filter in
			let vars_str = String.concat "/" (List.map OpamVariable.Full.to_string vars) in
			Util.debug "  # Ignoring package %s-%s (incompatible with %s)\n" package (Repo.string_of_version version) vars_str
		)
	);
	let opams = !opams in
	let ocaml_version = Version.of_string ocaml_version in
	let base_packages = ("ocaml" :: base_packages)
		|> List.map (fun name -> OpamPackage.create (Name.of_string name) ocaml_version)
		|> OpamPackage.Set.of_list
	in
	let get_depends depends =
		OpamPackage.Map.map (fun opam ->
			OpamFilter.partial_filter_formula env (depends opam)
		) opams
	in
	{ OpamSolver.empty_universe with
		u_packages  = OpamPackage.Set.empty;
		u_action    = Install;
		u_installed = base_packages;
		u_base      = base_packages;
		u_available = !available_packages;
		u_depends   = get_depends OpamFile.OPAM.depends;
		u_depopts   = get_depends OpamFile.OPAM.depopts;
		u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams
		              |> OpamPackage.Map.map (OpamFilter.filter_formula env);
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
	let ocaml_attr = ref None in
	let set_ocaml_attr arg = ocaml_attr := Some (Str.split (Str.regexp (Str.quote ".")) arg) in
	let ocaml_drv = ref None in
	let set_ocaml_drv arg = ocaml_drv := Some arg in
	let base_packages = ref "" in
	let opts = Arg.align [
		("--repo", Arg.String (fun repo -> repos := repo :: !repos), "Repository root");
		("--dest", Arg.Set_string dest, "Destination .nix file");
		("--ocaml-version", Arg.Set_string ocaml_version, "Target ocaml version");
		("--ocaml-attr", Arg.String set_ocaml_attr, "Ocaml nixpkgs attribute path (e.g `ocaml`, `ocaml-ng.ocamlPackages_4_05.ocaml`) (optional)");
		("--ocaml-drv", Arg.String set_ocaml_drv, "Concrete path to the ocaml derivation (.drv) to use (optional)");
		("--base-packages", Arg.Set_string base_packages, "Available base packages (comma-separated)");
		("--verbose", Arg.Set Util._verbose, "Verbose");
		("-v", Arg.Set Util._verbose, "Verbose");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse_argv ~current:(ref idx) args opts add_package "opam2nix: usage...";

	if !packages = [] then failwith "At least one package required";
	let packages = !packages in
	let dest = nonempty !dest "--dest" in
	let repos = nonempty_list !repos "--repo" in

	let () =
		if Util.verbose () then
			OpamCoreConfig.update ~debug_level:2 ()
	in

	let requested_packages : OpamFormula.atom list = packages |> List.map (fun spec ->
		let relop_re = Str.regexp "[!<=>]+" in
		match Str.full_split relop_re spec with
			| [Str.Text name; Str.Delim relop; Str.Text ver] ->
				let relop = OpamLexer.relop relop in
				let ver = OpamPackage.Version.of_string ver in
				(OpamPackage.Name.of_string name, Some (relop, ver))
			| [Str.Text name] -> (OpamPackage.Name.of_string name, None)
			| _ -> failwith ("Invalid version spec: " ^ spec)
	) in
	let package_names : OpamPackage.Name.t list = requested_packages |> List.map (fun (name, _) -> name) in

	let ocaml_version = nonempty !ocaml_version "--ocaml-version" in
	let ocaml_attr = !ocaml_attr in
	let ocaml_drv = !ocaml_drv in
	let base_packages = nonempty !base_packages "--base-packages" |> Str.split (Str.regexp ",") in
	let ocaml_attrs = let expr = match (ocaml_drv, ocaml_attr) with
		| Some _, Some _ -> failwith "both --ocaml and --ocaml-attribute provided"
		| Some drv, None -> Some (`Call [`Lit "import"; Nix_expr.str drv])
		| None, Some attr -> Some (`PropertyPath (`Id "self.pkgs", attr))
		| None, None ->
				Printf.eprintf
					"Note: neither --ocaml-attr nor --ocaml given; you will need to supply an `ocaml` attribute at import time";
				None
		in
		match expr with Some expr -> ["ocaml", expr] | None -> []
	in

	let universe = build_universe
		~repos:repos
		~base_packages
		~ocaml_version
		() in
	if Util.verbose () then print_universe stderr universe;
	(* let request = OpamSolver.request ~install:requested_packages in *)
	let request = {
		wish_install = requested_packages;
		wish_remove = [];
		wish_upgrade = [];
		extra_attributes = [];
		criteria = `Default;
	} in

	let () = OpamSolverConfig.init () in
	let () = (match OpamSolver.resolve universe ~orphans:OpamPackage.Set.empty request with
		| Success solution ->
				prerr_endline "Solved!";
				OpamSolver.print_solution
					~messages:(fun pkg -> [OpamPackage.to_string pkg])
					~append:(OpamPackage.to_string)
					~requested:(package_names |> OpamPackage.Name.Set.of_list)
					~reinstall:OpamPackage.Set.empty
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
				(* expose world.ocaml as a selection, so packages can access it like any other dep *)
				let selection = AttrSet.add "ocaml" (`Property (`Id "self", "ocaml")) selection in
				let selection = List.fold_right (fun base -> AttrSet.add base (`Lit "true")) base_packages selection in

				let attrs = [
					"ocamlVersion", str ocaml_version;
					"repositories", `List (repos |> List.map str);
					"selection", `Attrs selection
				] @ ocaml_attrs in

				let expr = `Function (
					`NamedArguments [`Id "super"; `Id "self"],
					(`Let_bindings (AttrSet.build [
						"opamPackages", `Property (`Id "self", "opamPackages");
					], `Attrs (AttrSet.build attrs)));
				) in
				let oc = open_out dest in
				Nix_expr.write oc expr;
				close_out oc
		| Conflicts conflict ->
			prerr_endline (
				OpamCudf.string_of_conflict (universe.u_available)
					(fun (p, version_formula) -> (
						"package " ^ (OpamPackage.Name.to_string p)
						^ " version " ^ (
							OpamFormula.string_of_formula
								(fun (op, ver) -> (OpamPrinter.relop op) ^ (OpamPackage.Version.to_string ver))
								version_formula
						) ^ " unavailable"
					))
					conflict
			);
			exit 1
	) in
	()
