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
	name: package_name;
	constraints: package_constraint list;
} [@@deriving of_yojson]

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

let solve : request -> spec = fun { req_repositories; req_selection } ->
	match req_selection with
		| Solve _ -> failwith "TODO"
		| Exact pset -> { spec_repositories = req_repositories; spec_packages = pset }

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
		p_vars = Opam_metadata.init_variables ();
	} in
	let opam = loaded.Repo.p_opam in
	let lookup_var = Vars.lookup_partial vars (OpamPackage.name pkg) in
	let resolve_commands =
		let open OpamFilter in
		let open OpamTypes in

		(*
		let open OpamTypesBase in
		let resolve_ident_raw ?(no_undef_expand=false) env fident =
			let open OpamStd.Option.Op in
			let packages,var,converter = fident in
			let bool_of_value = function
				| B b -> Some b
				| S s | L [s] ->
					(try Some (bool_of_string s) with Invalid_argument _ -> None)
				| L _ -> None
			in
			let resolve name =
				let var = match name with
					| Some n -> OpamVariable.Full.create n var
					| None -> OpamVariable.Full.self var
				in
				env var
			in
			let value_opt : variable_contents option = match packages with
			| [] -> env (OpamVariable.Full.global var)
			| [name] -> resolve name
			| names ->
				List.fold_left (fun acc name ->
						if acc = Some false then acc else
						match resolve name with
						| Some (B true) -> acc
						| v -> v >>= bool_of_value)
					(Some true) names
				>>| fun b -> B b
			in
			match converter, no_undef_expand with
			| Some (iftrue, iffalse), false ->
				(match value_opt >>= bool_of_value with
				 | Some true -> Some (S iftrue)
				 | Some false -> Some (S iffalse)
				 | None -> Some (S iffalse))
			| _ -> value_opt
		in

		let resolve_ident ?no_undef_expand env fident =
			match resolve_ident_raw ?no_undef_expand env fident with
			| Some (B b) -> FBool b
			| Some (S s) -> FString s
			| Some (L l) -> FString (String.concat " " l)
			| None -> FUndef (FIdent fident)
		in

		let string_interp_regex =
			let open Re in
			let notclose =
				rep (alt [
						diff notnl (set "}");
						seq [char '}'; alt [diff notnl (set "%"); stop] ]
					])
			in
			compile (alt [
					str "%%";
					seq [str "%{"; group (greedy notclose); opt (group (str "}%"))];
				])
		in

		let escape_expansions =
			Re.replace_string Re.(compile @@ char '%') ~by:"%%"
		in

		let value_string ?default = function
			| FBool b -> string_of_bool b
			| FString s -> s
			| FUndef f ->
				(match default with
				 | Some d -> d
				 | None -> failwith ("Undefined string filter value: "^to_string f))
			| e -> raise (Invalid_argument ("value_string: "^to_string e))
		in

		(* Resolves ["%{x}%"] string interpolations *)
		let expand_string_aux ?(partial=false) ?(escape_value=fun x -> x) ?default env text =
			let default fident = match default, partial with
				| None, false -> None
				| Some df, false -> Some (df fident)
				| None, true -> Some (Printf.sprintf "%%{%s}%%" fident)
				| Some df, true -> Some (Printf.sprintf "%%{%s}%%" (df fident))
			in
			let env v =
				if partial then
					match env v with
					| Some (S s) -> Some (S (escape_expansions s))
					| x -> x
				else env v
			in
			let f g =
				let str = Re.Group.get g 0 in
				if str = "%%" then (if partial then "%%" else "%")
				else if not (OpamStd.String.ends_with ~suffix:"}%" str) then
					(Printf.eprintf "ERR: Unclosed variable replacement in %S\n" str; str)
				else
				let fident = String.sub str 2 (String.length str - 4) in
				resolve_ident ~no_undef_expand:partial env (filter_ident_of_string fident)
				|> value_string ?default:(default fident) |> escape_value
			in
			Re.replace string_interp_regex ~f text
		in

		let expand_string = expand_string_aux ?escape_value:None in
		*)

		let expand_string ?partial env s  =
			let r = expand_string ?partial env s in
			Util.debug "Expanded -> %s\n" r;
			r
		in


		let arguments env (a,f) =
			if opt_eval_to_bool env f then
				let str = match a with
					| CString s -> s
					| CIdent i -> "%{"^i^"}%"
				in
				Util.debug "expanding string: %s\n" str;
				[expand_string ~partial:true env str]
					(*
					let fident = filter_ident_of_string i in
					match resolve_ident_raw ~no_undef_expand:true env fident with
					| Some (S s) -> [s]
					| Some (B b) -> [string_of_bool b]
					| Some (L sl) -> sl
					| None -> ["%{"^i^"}%"] (* can't resolve it, punt to runtime *)
					*)
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
