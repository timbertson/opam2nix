open Util
open OpamTypes
module Name = OpamPackage.Name
module Version = OpamPackage.Version

let build_universe ~repo ~packages () =
	let empty = OpamPackage.Set.empty in
	let names n = Name.Set.of_list (n |> List.map Name.of_string) in
	let all_packages = ref empty in
	let opams = ref OpamPackage.Map.empty in
	Repo.traverse `Nix ~repo ~packages:[] (fun package version path ->
		let opam = Opam_metadata.load_opam (Filename.concat path "opam") in
		let pkg = OpamPackage.create (Name.of_string package) (Version.of_string version) in
		all_packages := OpamPackage.Set.add pkg !all_packages;
		opams := OpamPackage.Map.add pkg opam !opams;
	);
	let opams = !opams in
	{
		u_packages  = empty;
		u_action    = Install (names packages); (* XXX this duplicates "atom request" below *)
		u_installed = empty;
		u_available = !all_packages;
		u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
		u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
		u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
		u_installed_roots = empty;
		u_pinned    = empty;
		(* u_dev       = empty; *)
		u_base      = [
			(* XXX hardcoded *)
			"base-unix";
			"base-bigarray";
			"base-threads";
		] |> List.map
				(* XXX is a real version useful? *)
				(fun name -> OpamPackage.create (Name.of_string name) (Version.of_string "0.0.0"))
			|> OpamPackage.Set.of_list; (* XXX *)
		(* u_attrs     = []; *)
		(* u_test      = false; *)
		(* u_doc       = false; *)
	}

let () =
	let repo = ref "" in
	let dest = ref "" in
	let opts = Arg.align [
		("--repo", Arg.Set_string repo, "Repository root");
		("--dest", Arg.Set_string dest, "Destination .nix file");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse opts add_package "TODO: usage...";

	if !packages = [] then failwith "At least one package required";
	let dest = nonempty !dest "--dest" in
	let package_names = !packages |> List.map OpamPackage.Name.of_string in

	let universe = build_universe ~repo:!repo ~packages:!packages () in
	let request = {
		wish_install = package_names |> List.map (fun name -> name, None); (* XXX version *)
		wish_remove = [];
		wish_upgrade = [];
		criteria = `Default;
	} in
	(* ignore (request, universe); *)
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
				let expr = (`Function (
					`NamedArguments [`Id "pkgs"; `Id "opam2nix"; `Id "opamPackages"; `Default ("builder",
						`Lit "opamSelection: pkg: pkgs.callPackage pkg { inherit opamSelection opam2nix; }"
					)],
					`Let_bindings (
						AttrSet.build ([ "selection", `Attrs selection ]),
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
