module JSON = Yojson.Safe
open Util

(* I can't find string_of_yojson function directly, so let's summon one by magic :) *)
type _str = string [@@deriving yojson]

type package_name = OpamPackage.Name.t
let package_name_of_yojson j = _str_of_yojson j |> Result.map OpamPackage.Name.of_string

type _package_raw = { name : string; version: string; } [@@deriving yojson]
let package_of_yojson j = _package_raw_of_yojson j |> Result.map (fun { name; version } ->
	OpamPackage.create (OpamPackage.Name.of_string name) (OpamPackage.Version.of_string version)
)

type package_set = OpamPackage.Set.t
let package_set_of_yojson = function
	| `List items ->
		List.fold_left (fun acc p ->
			Result.bind (fun acc ->
				(package_of_yojson p) |> Result.map (fun p -> p :: acc)
			) acc
		) (Ok []) items
		|> Result.map OpamPackage.Set.of_list
	| other -> Error ("Expected list of packages, got " ^ JSON.to_string other)

type repository = {
	repository_id: string [@key "id"];
	local_path: string [@key "path"];
} [@@deriving yojson]

type package_constraint = {
	c_op: string [@key "op"];
	c_value: string [@key "value"];
} [@@deriving yojson]

type package_spec = {
	s_name: package_name [@key "name"];
	constraints: package_constraint list [@default []];
} [@@deriving of_yojson]

type solve_ctx = {
	c_lookup_var: OpamPackage.t -> OpamVariable.Full.t -> OpamVariable.variable_contents option;
	c_constraints: OpamFormula.version_constraint option OpamPackage.Name.Map.t;
	c_repo_paths: string list;
	c_packages : (Repo.lookup_result, Solver.error) result OpamPackage.Map.t ref;
}

type selection =
	| Solve of package_spec list
	| Exact of package_set

type request_json = {
	rj_repositories: repository list [@key "repositories"];
	rj_spec: package_spec list option [@key "spec"][@default None];
	rj_selection: package_set option [@key "selection"][@default None];
} [@@deriving of_yojson]

type request = {
	req_repositories: repository list;
	req_selection: selection;
}

type spec = {
	spec_repositories: repository list [@key "repositories"];
	spec_packages: package_set [@key "packages"];
} [@@deriving of_yojson]

type buildable = {
	name: string;
	version: string;
	repository: string;
	src: Opam_metadata.url option;
	build_commands: string list list;
	install_commands: string list list;
} [@@deriving to_yojson]

let parse_request : JSON.t -> request = fun json ->
	request_json_of_yojson json |> Result.bind (fun { rj_repositories = req_repositories; rj_spec; rj_selection } ->
		match (rj_selection, rj_spec) with
			| (None, Some spec) -> Ok { req_repositories; req_selection = Solve spec }
			| (Some selection, None) -> Ok { req_repositories; req_selection = Exact selection }
			| _other -> Error "exactly one of spec or selection required"
	) |> Result.get_exn identity

let init_variables () = Opam_metadata.init_variables ()
	(* TODO don't add this in the first place *)
	(* TODO accept k/v pairs in request *)
	|> OpamVariable.Full.Map.remove (OpamVariable.Full.global (OpamVariable.of_string "jobs"))

module Zi = Opam_0install
module Context : Zi.S.CONTEXT with type t = solve_ctx = struct
	open Repo
	type t = solve_ctx
	type rejection = Solver.error

	(* TODO reuse Solver.pp_rejection *)
	let pp_rejection f = function
		| `unavailable s -> Fmt.pf f "Unavailable: %s" s
		| `unsupported_archive s -> Fmt.pf f "Unsupported archive: %s" s
		
	let check_url pkg =
		pkg.p_url
			|> Option.map Opam_metadata.url
			|> Option.sequence_result

	let candidates : t -> OpamPackage.Name.t -> (OpamPackage.Version.t * (OpamFile.OPAM.t, rejection) Stdlib.result) list
	= fun ctx name ->
		let name_str = Name.to_string name in
		let () = ctx.c_repo_paths |> List.concat_map (fun repo -> Repo.lookup_package_versions repo name_str)
			(* Drop duplicates from multiple repos *)
			|> List.filter (fun pkg -> not (OpamPackage.Map.mem pkg.p_package !(ctx.c_packages)))
			|> List.iter (fun pkg ->
				check_url pkg |> Result.iter (fun _ ->
					let result = Solver.is_available ~lookup_var:(ctx.c_lookup_var) ~opam:pkg.p_opam ~package:pkg.p_package
						|> Result.map (fun () -> pkg)
					in
					ctx.c_packages :=  OpamPackage.Map.add pkg.p_package result !(ctx.c_packages)
				)
			)
		in
		
		OpamPackage.Map.bindings !(ctx.c_packages)|> List.filter_map (fun (k,v) ->
			if Name.equal (OpamPackage.name k) name
				then Some (k.version, v |> Result.map(fun pkg -> pkg.p_opam))
				else None
		) |> List.sort (fun (va, _) (vb, _) ->
			Version.compare vb va
		)
		
	let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
	= fun ctx name -> OpamPackage.Name.Map.find name ctx.c_constraints

	let filter_deps : t -> OpamPackage.t -> OpamTypes.filtered_formula -> OpamTypes.formula
	= fun ctx pkg f ->
		f
		|> OpamFilter.partial_filter_formula (ctx.c_lookup_var pkg)
		|> OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false ~dev:false ~default:false
end

let solve : request -> spec = fun { req_repositories; req_selection } ->
	match req_selection with
		| Exact pset -> { spec_repositories = req_repositories; spec_packages = pset }
		| Solve specs ->
			let module Solver = Zi.Solver.Make(Context) in

			let lookup_var package =
				Vars.(lookup_partial {
					(* TODO ocaml package ? *)
					p_packages = OpamPackage.Name.Map.empty;
					p_vars = init_variables ();
				}) (OpamPackage.name package)
			in
			let ctx = {
				c_repo_paths = req_repositories |> List.map (fun r -> r.local_path);
				c_packages = ref OpamPackage.Map.empty;
				c_constraints = OpamPackage.Name.Map.empty; (* TODO *)
				c_lookup_var = lookup_var;
			} in
			let package_names = specs |> List.map (fun spec -> spec.s_name) in
			(match Solver.solve ctx package_names with
				| Error e -> (
					prerr_endline (Solver.diagnostics e);
					exit 1
				)
				| Ok solution ->
					let installed = Solver.packages_of_result solution in
					Printf.eprintf "Selected packages:\n";
					installed |> List.iter (fun pkg -> Printf.eprintf "- %s\n" (OpamPackage.to_string pkg));
					{
						spec_repositories = req_repositories;
						spec_packages = installed |> OpamPackage.Set.of_list;
					}
			)

let lookup : OpamPackage.t -> repository -> Repo.lookup_result option = fun pkg repo ->
	Repo.lookup repo.local_path pkg
	
let find_impl : OpamPackage.t -> repository list -> repository * Repo.lookup_result = fun pkg ->
	let rec search = function
		| [] -> failwith ("Package not found in any repository: " ^ (OpamPackage.to_string pkg))
		| repository::tail -> (match lookup pkg repository with
			| Some found -> (repository, found)
			| None -> search tail
		)
	in search
	
let buildable : package_set -> OpamPackage.t -> (repository * Repo.lookup_result) -> buildable = fun installed pkg (repo, loaded) ->
	let url = loaded.Repo.p_url
		|> Option.map Opam_metadata.url
		|> Option.map (Result.get_exn Opam_metadata.string_of_unsupported_archive)
	in
	let vars = Vars.{
		p_packages = installed
			|> OpamPackage.Set.elements
			|> List.map (fun p -> (OpamPackage.name p, p))
			|> OpamPackage.Name.Map.of_list;
		p_vars = init_variables ();
	} in
	let opam = loaded.Repo.p_opam in
	let lookup_var = Vars.lookup_partial vars (OpamPackage.name pkg) in
	let resolve_commands =
		let open OpamFilter in
		let open OpamTypes in

		let arguments env (a,f) =
			if opt_eval_to_bool env f then
				let str = match a with
					| CString s -> s
					| CIdent i -> "%{"^i^"}%"
				in
				Util.debug "expanding string: %s\n" str;
				[expand_string ~partial:true env str]
			else
				[]
		in
		let command env (l, f) =
			if opt_eval_to_bool env f then
				match List.concat (List.map (arguments env) l) with
				| [] -> None
				| l  -> Some l
			else
				None
		in
		let commands env l = OpamStd.List.filter_map (command env) l in
		commands lookup_var
	in
	{
		name = OpamPackage.Name.to_string (OpamPackage.name pkg);
		version = OpamPackage.Version.to_string (OpamPackage.version pkg);
		repository = repo.repository_id;
		src = url;
		build_commands =
			OpamFile.OPAM.build opam |> resolve_commands;
		install_commands =
			OpamFile.OPAM.install opam |> resolve_commands;
	}

let dump : spec -> JSON.t = fun { spec_repositories; spec_packages } ->
	let buildable = spec_packages
		|> OpamPackage.Set.elements
		|> List.map (fun pkg -> buildable spec_packages pkg (find_impl pkg spec_repositories))
		|> List.sort (fun a b -> String.compare a.name b.name)
	in
	`Assoc (buildable |> List.map (fun buildable ->
		(buildable.name, buildable_to_yojson buildable)
	))

let run () =
	let%lwt json_s = Lwt_io.read Lwt_io.stdin in
	json_s
		|> JSON.from_string
		|> parse_request
		|> solve
		|> dump
		|> JSON.pretty_to_string
		|> Lwt_io.printf "%s\n"

let main _idx _args = Lwt_main.run (run ())
