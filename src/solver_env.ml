open Util
module Name = OpamPackage.Name
module Version = OpamPackage.Version
module OPAM = OpamFile.OPAM
module Zi = Opam_0install

(* external constraints are the preselected boundaries of
 * the selection - what repos and compiler we're using *)
type external_constraints = {
	ocaml_version: Version.t;
	repos: Repo.t list;
}

(* an opam_source is a package from the opam repo *)
type loaded_package = {
	opam: OPAM.t;
	repository_expr: unit -> Opam_metadata.opam_src Lwt.t;
	src_expr: Digest_cache.t -> (Nix_expr.t option, Digest_cache.error) Result.t Lwt.t;
	url: Opam_metadata.url option;
}

(* a direct package is passed on the commandline, and is
 * not from any repository *)
type direct_package = {
	direct_opam_relative: string;
	direct_opam: OPAM.t;
	direct_name: Name.t;
	direct_version: Version.t option;
}

type error = [
	Opam_metadata.unsupported_archive
	| `unavailable of string
]

type universe = {
	lookup_var : OpamPackage.t -> OpamVariable.Full.t -> OpamVariable.variable_contents option;
	repos : Repo.t list;
	packages : (loaded_package, error) result OpamPackage.Map.t ref;
	(* direct_packages : direct_package list; *)
}

let nix_digest_of_path p =
	let hash_cmd = [ "nix-hash"; "--type"; "sha256"; "--flat"; "--base32"; "/dev/stdin" ] in
	let (readable, writeable) = Unix.pipe ~cloexec:true () in
	let open Cmd in
	run_exn (exec_r ~stdin:(`FD_move readable)) ~print:false ~block:(fun hash_proc ->
		Lwt.both
			(file_contents (hash_proc#stdout))
			(run_unit_exn (exec_none ~stdout:(`FD_move writeable)) ~print:false [ "nix-store"; "--dump"; p ])
			|> Lwt.map (fun (output, ()) -> output)
	) hash_cmd
	|> Lwt.map (fun hash -> `sha256 hash)

let build_universe ~external_constraints ~base_packages ~direct_packages () : universe =
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
		

	let initial_packages = direct_packages |> List.map (fun package ->
		let name = package.direct_name in
		let version = package.direct_version |> Option.default (Version.of_string "development") in
		(
			(OpamPackage.create name version),
			Ok {
				opam = package.direct_opam;
				url = None;
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
	}

let load_package ~url pkg : loaded_package = (
	let src_expr = fun cache ->
		url |> Option.map (fun url ->
			Opam_metadata.nix_of_url ~cache url |> Lwt.map (Result.map Option.some)
		) |> Option.default (Lwt.return (Ok None))
	in
	let repository_expr () = (
		nix_digest_of_path (Repo.full_path pkg)
		|> Lwt.map (fun (`sha256 digest) ->
			let digest = "sha256:" ^ digest in
			let open Repo in
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
	{ opam = pkg.opam; url; src_expr; repository_expr }
)

let check_availability ~lookup_var pkg =
	let open Repo in
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
	
	
module Solver_context : Zi.S.CONTEXT = struct
	open Repo
	
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
		env.repos |> List.concat_map (fun repo -> Repo.list_package repo name_str)
			(* Drop duplicates from multiple repos *)
			|> List.filter (fun pkg -> not (OpamPackage.Map.mem pkg.package !(env.packages)))
			|> List.map (fun pkg ->
				let version = pkg.package |> OpamPackage.version in
				let opam = check_url pkg |> Result.bind (fun url ->
					let result = check_availability ~lookup_var:(env.lookup_var) pkg in
					let loaded = result |> Result.map (load_package ~url) in
					env.packages :=  OpamPackage.Map.add pkg.package loaded !(env.packages);
					result |> Result.map (fun pkg -> pkg.opam)
				) in
				(version, opam)
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
module Solver = Zi.Solver.Make(Zi.Dir_context)

let test ~external_constraints ~package_names =
	let env =
		Zi.Dir_context.std_env
			~arch:"x86_64"
			~os:"linux"
			~os_family:"debian"
			~os_distribution:"debian"
			~os_version:"10"
			() in

	let first_repo_dir = (external_constraints.repos |> List.hd).repo_path ^ "/packages" in
	print_endline first_repo_dir;
	let context =
		Zi.Dir_context.create first_repo_dir
			~constraints:OpamPackage.Name.Map.empty
			~env in

	let result = Solver.solve context package_names in
	let () = match result with
	| Error e -> print_endline (Solver.diagnostics e)
	| Ok selections ->
		Solver.packages_of_result selections
		|> List.iter (fun pkg -> Printf.printf "- %s\n" (OpamPackage.to_string pkg))
	in
	if (true) then exit 1;