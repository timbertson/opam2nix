module JSON = Yojson.Safe
open Util
module Name = OpamPackage.Name
module Version = OpamPackage.Version
module OPAM = OpamFile.OPAM

type package_name = OpamPackage.Name.t
let package_name_of_yojson j = [%of_yojson: string] j |> Result.map OpamPackage.Name.of_string
let package_name_to_yojson name = `String (Name.to_string name)

type package_version = OpamPackage.Version.t
let package_version_of_yojson j = [%of_yojson: string] j |> Result.map OpamPackage.Version.of_string

type _package_raw = { name : string; version: string; } [@@deriving yojson]
type package = OpamPackage.t
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
}

let package_constraint_of_yojson j = j
	|> [%of_yojson: string * string]
	|> Result.map (fun (c_op, c_value) -> { c_op; c_value })

type opam_source = Opam_file of string | Opam_contents of string

let string_of_opam_source = function Opam_file s -> s | Opam_contents _ -> "[literal string]"

type package_definition =
	| From_repository
	(* path may be either an opam file or containing directory *)
	| Direct of opam_source
	
type opam_source_json = {
	osj_contents: string [@key "contents"];
} [@@deriving of_yojson]

let package_definition_of_yojson: JSON.t -> (package_definition, string) result = function
	| `Null -> Ok From_repository
	| `String s -> Ok (Direct (Opam_file s))
	| `Assoc j -> [%of_yojson: opam_source_json] (`Assoc j)
			|> Result.map (fun j -> Direct (Opam_contents j.osj_contents))
	| _ -> Error "package_definition"

type package_spec = {
	s_name: package_name option [@key "name"] [@default None];
	s_definition: package_definition [@key "definition"] [@default From_repository];
	constraints: package_constraint list [@default []];
} [@@deriving of_yojson]

type lazy_definition =
	| Definition of package_definition
	| Loaded_definition of Repo.lookup_result

(* JSON can't represent loaded definitions *)
let lazy_definition_of_yojson j = [%of_yojson: package_definition] j |> Result.map (fun d -> Definition d)

type selected_package = {
	sel_name: package_name [@key "name"];
	sel_version: package_version [@key "version"];
	sel_definition: lazy_definition [@key "definition"] [@default (Definition From_repository)];
} [@@deriving of_yojson]

let package_of_selected { sel_name; sel_version; _ } =
	OpamPackage.create sel_name sel_version

type selected_package_map = selected_package OpamPackage.Name.Map.t

let selected_package_map_of_yojson j = j
	|> [%of_yojson: selected_package list]
	|> Result.map (fun selected ->
		selected
		|> List.map (fun sel -> sel.sel_name, sel)
		|> OpamPackage.Name.Map.of_list
	)

type solve_ctx = {
	c_lookup_var: OpamPackage.t -> OpamVariable.Full.t -> OpamVariable.variable_contents option;
	c_constraints: OpamFormula.version_constraint option OpamPackage.Name.Map.t;
	c_repo_paths: string list;
	c_preloaded: Repo.lookup_result OpamPackage.Name.Map.t;
	c_packages : (Repo.lookup_result, Solver.error) result OpamPackage.Map.t ref;
}

type selection =
	| Solve of package_spec list
	| Exact of selected_package_map

type request_json = {
	rj_repositories: repository list [@key "repositories"];
	rj_spec: package_spec list option [@key "spec"][@default None];
	rj_selection: selected_package_map option [@key "selection"][@default None];
} [@@deriving of_yojson]

type request = {
	req_repositories: repository list;
	req_selection: selection;
}

type spec = {
	spec_repositories: repository list [@key "repositories"];
	spec_packages: selected_package_map [@key "packages"];
}

let url_to_yojson = function
	| Some (`http (url, _digests)) -> `Assoc ["url", `String url]
	| None -> `Null

type depexts = {
	required: string list;
	optional: string list;
} [@@deriving to_yojson]

let empty_depexts = { optional = []; required = [] }

type buildable = {
	version: string;
	repository: string option;
	src: Opam_metadata.url option [@to_yojson url_to_yojson];
	build_commands: string list list;
	install_commands: string list list;
	depends: package_name list [@default []];
	depexts: depexts [@default empty_depexts];
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
	
let load_direct ~name opam_source : Repo.lookup_result =
	let (opam, opam_dir, opam_filename) = match opam_source with
		| Opam_file path -> (
			let isdir = Sys.is_directory path in
			let opam_dir = if isdir then Some(path) else None in
			let opam_path = if isdir
				then Filename.concat path "opam"
				else path
			in

			let opam = Opam_metadata.load_opam opam_path in

			let basename = Filename.basename path in
			opam, opam_dir, Some(basename)
		)
		| Opam_contents c -> (
			let opam = Opam_metadata.load_opam_string c in
			opam, None, None
		)
	in

	let name = name |> Option.or_else (OPAM.name_opt opam) |> Option.or_else_fn (fun () ->
		(* Name can only be missing for a file called `[pkgname].opam` *)
		opam_filename
		|> Option.bind (function filename ->
			let stripped = OpamStd.String.remove_suffix ~suffix:".opam" filename in
			if stripped <> filename then
				Some (Name.of_string stripped)
			else
				None
		)
	) |> Option.or_failwith ("Couldn't determine package name from " ^ (string_of_opam_source opam_source))
	in

	let version = OPAM.version_opt opam |> Option.or_else_fn (fun () ->
		(* If the path happens to be a directory named foo.1.2.3, treat that
		as the fallback version *)
		opam_dir |> Option.map Filename.basename |> Option.bind (fun basename ->
			let prefix = (Name.to_string name) ^ "." in
			let stripped = OpamStd.String.remove_prefix ~prefix basename in
			if stripped <> basename then
				Some (Version.of_string (OpamStd.String.remove_suffix ~suffix:".opam" stripped))
			else None
		)
	) |> Option.default (Version.of_string "dev")
	in

	let url = OPAM.url opam |> Option.or_else_fn (fun () ->
		(* TODO what if there is no url? *)
		opam_dir |> Option.bind (fun dir -> Repo.load_url (Filename.concat dir "url"))
	) in

	Repo.{
		p_package = OpamPackage.create name version;
		p_rel_path = "UNUSED"; (* TODO: remove? *)
		p_opam = opam;
		p_url = url;
	}

module Zi = Opam_0install
module Context : Zi.S.CONTEXT with type t = solve_ctx = struct
	open Repo
	type t = solve_ctx
	type rejection = Solver.error

	(* TODO reuse Solver.pp_rejection *)
	let pp_rejection f = function
		| `unavailable s -> Fmt.pf f "Unavailable: %s" s
		| `unsupported_archive s -> Fmt.pf f "Unsupported archive: %s" s
		
	let check_url pkg : (Opam_metadata.url option, rejection) Stdlib.result =
		pkg.p_url
			|> Option.map Opam_metadata.url
			|> Option.sequence_result

	let candidates : t -> OpamPackage.Name.t -> (OpamPackage.Version.t * (OpamFile.OPAM.t, rejection) Stdlib.result) list
	= fun ctx name ->
		let name_str = Name.to_string name in
		(* TODO is this caching actually useful? ZI probably does it *)
		let version loaded = OpamPackage.version loaded.p_package in
		match OpamPackage.Name.Map.find_opt name ctx.c_preloaded with
			| Some loaded -> [ version loaded, (Ok loaded.p_opam) ]
			| None ->
				let seen = ref Version.Set.empty in
				ctx.c_repo_paths
					|> List.concat_map (fun repo -> Repo.lookup_package_versions repo name_str)
					|> List.filter_map (fun loaded ->
						(* Drop duplicates from multiple repos *)
						if (Version.Set.mem (version loaded) !seen) then None else (
							let opam = check_url loaded |> Result.bind (fun _ ->
								Solver.is_available ~lookup_var:(ctx.c_lookup_var) ~opam:loaded.p_opam ~package:loaded.p_package
							) |> Result.map (fun () ->
								(* only mark a package as seen if it's available *)
								seen := Version.Set.add (version loaded) !seen;
								loaded.p_opam
							) in
							Some (version loaded, opam)
						)
					)
					|> List.sort (fun (va, _) (vb, _) -> Version.compare vb va)
		
	let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
	= fun ctx name -> OpamPackage.Name.Map.find_opt name ctx.c_constraints |> Option.bind identity

	let filter_deps : t -> OpamPackage.t -> OpamTypes.filtered_formula -> OpamTypes.formula
	= fun ctx pkg f ->
		f
		|> OpamFilter.partial_filter_formula (ctx.c_lookup_var pkg)
		|> OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false ~dev:false ~default:false
end

let solve : request -> spec = fun { req_repositories; req_selection } ->
	match req_selection with
		| Exact pmap -> { spec_repositories = req_repositories; spec_packages = pmap }
		| Solve specs ->
			Printf.eprintf "Solving ...\n";
			flush stderr;
			let module Solver = Zi.Solver.Make(Context) in

			let lookup_var package =
				Vars.(lookup_partial {
					(* TODO preload "ocaml" package ? *)
					p_packages = OpamPackage.Name.Map.empty;
					p_vars = init_variables ();
				}) (OpamPackage.name package)
			in
			let loaded_definitions = specs
				|> List.filter_map (fun spec ->
						match spec.s_definition with
							| From_repository -> None
							| Direct path ->
								let loaded = load_direct ~name:spec.s_name path in
								Some (OpamPackage.name loaded.p_package, loaded)
				) |> OpamPackage.Name.Map.of_list
			in
			let direct_names = Name.Map.keys loaded_definitions in
			let repo_names = specs |> List.filter_map (fun spec ->
				match spec.s_definition with
					| From_repository -> Some (spec.s_name |> Option.or_failwith "Package name or definition required")
					| Direct _ -> None
			) in
			let ctx = {
				c_repo_paths = req_repositories |> List.map (fun r -> r.local_path);
				c_preloaded = loaded_definitions;
				c_packages = ref OpamPackage.Map.empty;
				c_constraints = OpamPackage.Name.Map.empty; (* TODO *)
				c_lookup_var = lookup_var;
			} in
			(match Solver.solve ctx (direct_names @ repo_names) with
				| Error e -> (
					prerr_endline (Solver.diagnostics e);
					exit 1
				)
				| Ok solution ->
					let installed = Solver.packages_of_result solution in
					Printf.eprintf "Selected packages:\n";
					installed |> List.iter (fun pkg -> Printf.eprintf "- %s\n" (OpamPackage.to_string pkg));
					flush stderr;
					{
						spec_repositories = req_repositories;
						spec_packages = installed |>
							List.map (fun p ->
								let sel_name = OpamPackage.name p in
								let sel_definition = loaded_definitions
									|> OpamPackage.Name.Map.find_opt sel_name
									|> Option.fold
										(fun loaded -> Loaded_definition loaded)
										(fun () -> Definition From_repository)
								in
								(sel_name, {
									sel_name;
									sel_version = OpamPackage.version p;
									sel_definition;
								})
							)
							|> OpamPackage.Name.Map.of_list
					}
			)

let find_impl : selected_package -> repository list -> repository option * Repo.lookup_result = fun pkg repos ->
	match pkg.sel_definition with
		| Loaded_definition loaded -> (None, loaded)
		| Definition (Direct path) -> (None, load_direct ~name:(Some pkg.sel_name) path)
		| Definition (From_repository) -> (
			let lookup : selected_package -> repository -> Repo.lookup_result option = fun pkg repo ->
				Repo.lookup repo.local_path (package_of_selected pkg)
			in
			let rec search = function
				| [] -> failwith ("Package not found in any repository: " ^ (OpamPackage.to_string (package_of_selected pkg)))
				| repository::tail -> (match lookup pkg repository with
					| Some found -> (Some repository, found)
					| None -> search tail
				)
			in search repos
		)
	
let buildable : selected_package_map -> selected_package -> (repository option * Repo.lookup_result) -> buildable = fun installed pkg (repo, loaded) ->
	let url = loaded.Repo.p_url
		|> Option.map Opam_metadata.url
		|> Option.map (Result.get_exn Opam_metadata.string_of_unsupported_archive)
	in
	let vars = Vars.{
		p_packages = installed |> OpamPackage.Name.Map.map package_of_selected;
		p_vars = init_variables ();
	} in
	let opam = loaded.Repo.p_opam in
	let lookup_var = Vars.lookup_partial vars pkg.sel_name in

	let depends =
		let of_formula formula =
			let rec accumulate_deps acc = let open OpamFormula in function
				| Empty    -> acc
				| Atom x   -> x :: acc
				| Block x  -> accumulate_deps acc x
				| And(x,y) | Or(x,y) -> (accumulate_deps [] x) @ (accumulate_deps acc y)
			in
			OpamPackageVar.filter_depends_formula
				~build:true
				~post:false
				~test:false
				~doc:false
				~default:false
				~env:lookup_var
				formula
			|> accumulate_deps []
			|> List.map (fun (name, _) -> name)
			|> List.filter (fun name -> Name.Map.mem name installed)
		in
		( of_formula (OPAM.depends opam)
		@ of_formula (OPAM.depopts opam)
		) |> List.sort_uniq Name.compare
	in
	let depexts =
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
		
		let merge_and_sort sets = sets
			|> List.concat_map OpamSysPkg.Set.elements
			|> List.map OpamSysPkg.to_string
			|> List.sort_uniq String.compare in

		let raw_depexts = OPAM.depexts opam in
		filter_map (apply_filters lookup_var) raw_depexts |> (function
			| [] -> { empty_depexts with optional = List.map fst raw_depexts |> merge_and_sort }
			| nixos_deps -> { empty_depexts with required = nixos_deps |> merge_and_sort }
		)
	in
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
		version = OpamPackage.Version.to_string pkg.sel_version;
		repository = repo |> Option.map (fun repo -> repo.repository_id);
		src = url;
		depends;
		depexts;
		build_commands = OpamFile.OPAM.build opam |> resolve_commands;
		install_commands = OpamFile.OPAM.install opam |> resolve_commands;
	}

let dump : spec -> JSON.t = fun { spec_repositories; spec_packages } ->
	let buildable = spec_packages
		|> OpamPackage.Name.Map.values
		|> List.map (fun (pkg: selected_package) ->
			(pkg.sel_name, buildable spec_packages pkg (find_impl pkg spec_repositories)))
		|> List.sort (fun (aname,_) (bname,_) -> Name.compare aname bname)
	in
	`Assoc (buildable |> List.map (fun (name, buildable) ->
		(Name.to_string name, buildable_to_yojson buildable)
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
