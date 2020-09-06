open Util
module Name = OpamPackage.Name
module Version = OpamPackage.Version
module OPAM = OpamFile.OPAM
module Zi = Opam_0install
open Repo

(* external constraints are the preselected boundaries of
 * the selection - what repos and compiler we're using *)
type external_constraints = {
	ocaml_version: Version.t;
	repos: Repo.t list;
}

type error = [
	Opam_metadata.unsupported_archive
	| `unavailable of string
]

type universe = {
	lookup_var : OpamPackage.t -> OpamVariable.Full.t -> OpamVariable.variable_contents option;
	repos : Repo.t list;
	packages : (loaded_package, error) result OpamPackage.Map.t ref;
	constrained_versions : Version.t Name.Map.t;
}

let build_universe ~external_constraints ~base_packages ~constrained_versions ~direct_definitions () : universe =
	let ocaml_version = external_constraints.ocaml_version in
	let global_vars = Opam_metadata.init_variables () in
	let lookup_var package =
		let version = OpamPackage.version package in
		let name = OpamPackage.name package in
		Vars.(lookup {
			ocaml_version;
			packages = (Name.Map.of_list (
				[
					name, (Installed {
						path = None; (* not yet known *)
						version = Some version;
					});
					Name.of_string "ocaml", (Installed {
						path = None; (* not yet known *)
						version = Some ocaml_version;
					})
				] @ (base_packages |> List.map (fun name -> (name, Provided)))
			));
			prefix = None; (* not known *)
			self = name;
			vars = global_vars;
		}) in
		

	let initial_packages = direct_definitions |> List.map (fun package ->
		let name = package.direct_name in
		let version = package.direct_version |> Option.default (Version.of_string "development") in
		(
			(OpamPackage.create name version),
			Ok {
				loaded_opam = package.direct_opam;
				loaded_url = None;
				src_expr = (fun _ -> Lwt.return (Ok (Some (
					`Call [`Lit "self.directSrc"; name |> Name.to_string |> Nix_expr.str]
				))));
				repository_expr = fun () -> Lwt.return (`File (Nix_expr.str package.direct_opam_relative));
			}
		)
	) |> OpamPackage.Map.of_list in

	{
		lookup_var = lookup_var;
		repos = external_constraints.repos;
		packages = ref initial_packages;
		constrained_versions = Name.Map.of_list constrained_versions;
	}

let load_package ~url pkg : loaded_package = (
	let src_expr = fun cache ->
		url |> Option.map (fun url ->
			Opam_metadata.nix_of_url ~cache url |> Lwt.map (Result.map Option.some)
		) |> Option.default (Lwt.return (Ok None))
	in
	let repository_expr () = (
		Repo.nix_digest_of_path (Repo.full_path pkg)
		|> Lwt.map (fun (`sha256 digest) ->
			let digest = "sha256:" ^ digest in
			`Dir (Nix_expr.(`Call [
				`Id "repoPath";
				`PropertyPath (`Id "repos", [pkg.repo.repo_key; "src"]);
				`Attrs (AttrSet.build [
					"package", str pkg.rel_path;
					"hash", str digest;
				])
			]))
		)
	) in
	{ loaded_opam = pkg.opam; loaded_url = url; src_expr; repository_expr }
)

let check_availability ~lookup_var pkg =
	let available_filter = OPAM.available pkg.opam in
	let available =
		try Ok (pkg.package.name <> (Name.of_string "opam")
			&& OpamFilter.eval_to_bool (lookup_var pkg.package) available_filter
		) with e -> (
			Error (`unavailable (Printexc.to_string e))
		)
	in
	available |> Result.bind (fun available ->
		if available then (
			Ok pkg
		) else (
			let vars = OpamFilter.variables available_filter in
			let vars_str = String.concat "/" (List.map OpamVariable.Full.to_string vars) in
			Error (`unavailable (Printf.sprintf "incompatible with %s" vars_str))
		)
	)
	
module Context : Zi.S.CONTEXT with type t = universe = struct
	type t = universe
	type rejection = error

	let pp_rejection f = function
		| `unavailable s -> Fmt.pf f "Unavailable: %s" s
		| `unsupported_archive s -> Fmt.pf f "Unsupported archive: %s" s
		
	let check_url pkg =
		pkg.url
			|> Option.map Opam_metadata.url
			|> Option.sequence_result

	let candidates : t -> OpamPackage.Name.t -> (OpamPackage.Version.t * (OpamFile.OPAM.t, rejection) Stdlib.result) list
	= fun env name ->
		let name_str = Name.to_string name in
		let () = env.repos |> List.concat_map (fun repo -> Repo.list_package repo name_str)
			(* Drop duplicates from multiple repos *)
			|> List.filter (fun pkg -> not (OpamPackage.Map.mem pkg.package !(env.packages)))
			|> List.filter (fun pkg ->
				Name.Map.find_opt pkg.package.name env.constrained_versions
					|> Option.map (Version.equal pkg.package.version) (* OK if versions equal *)
					|> Option.default true (* or if there is no constraint on the version *)
			)
			|> List.iter (fun pkg ->
				check_url pkg |> Result.iter (fun url ->
					let result = check_availability ~lookup_var:(env.lookup_var) pkg in
					let loaded = result |> Result.map (load_package ~url) in
					env.packages :=  OpamPackage.Map.add pkg.package loaded !(env.packages)
				)
			)
		in
		
		OpamPackage.Map.bindings !(env.packages)|> List.filter_map (fun (k,v) ->
			if Name.equal (OpamPackage.name k) name
				then Some (k.version, v |> Result.map(fun loaded -> loaded.loaded_opam))
				else None
		) |> List.sort (fun (va, _) (vb, _) ->
			Version.compare vb va
		)
		
	(* Not supported *)
	let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
	= fun _ _ -> None

	let filter_deps : t -> OpamPackage.t -> OpamTypes.filtered_formula -> OpamTypes.formula
	= fun env pkg f ->
		f
		|> OpamFilter.partial_filter_formula (env.lookup_var pkg)
		|> OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false ~dev:false ~default:false
end

include Zi.Solver.Make(Context)