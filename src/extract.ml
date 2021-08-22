module JSON = Yojson.Safe
open Util

type package_name = OpamPackage.Name.t
let package_name_of_yojson = Obj.magic

type package_set = OpamPackage.Set.t
let package_set_of_yojson = Obj.magic

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
	| Solve of package_spec list [@name "solve"]
	| Exact of package_set [@name "exact"]
	[@@deriving of_yojson]

type request = {
	req_repositories: repository list [@key "repositories"];
	req_selection: selection [@key "packages"];
} [@@deriving of_yojson]

type spec = {
	spec_repositories: repository list [@key "repositories"];
	spec_packages: package_set [@key "packages"];
} [@@deriving of_yojson]

type buildable = {
	name: string;
	version: string;
	src: Opam_metadata.url option;
	build_commands: string list list;
	install_commands: string list list;
}

let parse_request : JSON.t -> request = fun json ->
	request_of_yojson json |> Result.get_exn identity

let solve : request -> spec = fun { req_repositories; req_selection } ->
	match req_selection with
		| Solve _ -> failwith "TODO"
		| Exact pset -> { spec_repositories = req_repositories; spec_packages = pset }

let lookup : OpamPackage.t -> repository -> Repo.lookup_result option = fun pkg repo ->
	Repo.lookup repo.local_path pkg
	
let find_impl : OpamPackage.t -> repository list -> Repo.lookup_result = fun pkg ->
	let rec search = function
		| [] -> failwith ("Package not found in any repository: " ^ (OpamPackage.to_string pkg))
		| repository::tail -> (match lookup pkg repository with
			| Some found -> found
			| None -> search tail
		)
	in search
	
let buildable : OpamPackage.t -> Repo.lookup_result -> buildable = fun pkg loaded ->
	let url = loaded.Repo.p_url
		|> Option.map Opam_metadata.url
		|> Option.map (Result.get_exn Opam_metadata.string_of_unsupported_archive)
	in
	{
		name = OpamPackage.Name.to_string (OpamPackage.name pkg);
		version = OpamPackage.Version.to_string (OpamPackage.version pkg);
		src = url;
		build_commands = [["TODO"]];
		install_commands = [["TODO"]];
	}

let dump : spec -> JSON.t = fun { spec_repositories; spec_packages } ->
	let resolved = spec_packages
		|> OpamPackage.Set.elements
		|> List.map (fun pkg -> buildable pkg (find_impl pkg spec_repositories))
		(* |> OpamPackage.Map.of_list *)
		|> List.sort (fun a b -> String.compare a.name b.name)
	in
	ignore resolved;
	Obj.magic ()

let run () =
	let%lwt json_s = Lwt_io.read Lwt_io.stdin in
	json_s
		|> JSON.from_string
		|> parse_request
		|> solve
		|> dump
		|> JSON.pretty_to_string
		|> Lwt_io.printf "%s\n"

let main _idx _args =
	(*
	 * parse selection
	 * if necessary, solve for constraints
	 * now we have a solution, dump it into JSON
	 * include dependencies, repo source, build instructions and file list.
	 *)
	Lwt_main.run (run ())
