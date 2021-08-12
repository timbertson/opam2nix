type extract_repository = {
	repository_id: string,
	local_path: string,
}

type package_constraint = string

type package_spec = {
	package_name: OpamPackage.Name.t,
	package_constraint: package_constraint option,
}

type selection = [ Spec of package_spec list | Solution of OpamPackage.t list ]

type extract_request = {
	repositories: extract_repository list,
	selection: selection,
}

let main idx args =
	(*
	 * parse selection
	 * if necessary, solve for constraints
	 * now we have a solution, dump it into JSON
	 * include dependencies, repo source, build instructions and file list.
	 *)
	Printf.eprintf "TODO\n"
