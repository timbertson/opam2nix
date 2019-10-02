open Util
open OpamTypes
module Name = OpamPackage.Name
module Version = OpamPackage.Version
module OPAM = OpamFile.OPAM
open Cmd.Types

(* a direct package is passed on the commandline, and is
 * not from any repository *)
type direct_package = {
	direct_opam_relative: string;
	direct_opam: OPAM.t;
	direct_name: Name.t;
	direct_version: Version.t option;
}

(* an opam_source is a package from the opam repo *)
type loaded_package = {
	opam: OPAM.t;
	repository_expr: Opam_metadata.opam_src Lazy.t;
	url: Opam_metadata.url option Lazy.t;
}

(* external constraints are the preselected boundaries of
 * the selection - what repo and compiler we're using *)
type external_constraints = {
	repo_commit: string;
	ocaml_version: Version.t;
	repo_sha256: string;
}

let repo_owner = "ocaml"
let repo_name = "opam-repository"
let repo_url = Printf.sprintf "https://github.com/%s/%s.git" repo_owner repo_name

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

let nix_digest_of_path p =
	let hash_cmd = [ "nix-hash"; "--type"; "sha256"; "--flat"; "--base32"; "/dev/stdin" ] in
	let hash = Cmd.run_exn ~print:false ~stdin:Pipe ~stdout:Pipe ~block:(fun hash_proc ->
		let collector = Cmd.file_contents_in_bg (hash_proc.stdout |> Cmd.assert_fd) in
		let stdout = Fd (hash_proc.stdin |> Cmd.assert_fd) in
		Cmd.run_unit_exn ~print:false ~stdout [ "nix-store"; "--dump"; p ];
		Cmd.join_bg collector
	) hash_cmd in
	"sha256:" ^ hash

let unique_file_counter =
	let state = ref 0 in
	fun () -> (
		let ret = !state in
		incr state;
		ret
	)

let nix_digest_of_git_repo p =
	let print = false in
	let tempname = Printf.sprintf "opam2nix-%d-%d" (Unix.getpid ()) (unique_file_counter ()) in
	let tempdir = Filename.concat (Filename.get_temp_dir_name ()) tempname in
	Unix.mkdir tempdir 0o700;
	let cleanup () = rm_r tempdir in
	try
		Cmd.run_exn ~print ~stdin:Pipe ~block:(fun proc ->
			let stdout = Fd (proc.stdin |> Cmd.assert_fd) in
			Cmd.run_unit_exn ~print ~stdout
				[ "git"; "-C"; p; "archive"; "HEAD" ]
		) [ "tar"; "x"; "-C"; tempdir ];
		let ret = nix_digest_of_path tempdir in
		cleanup ();
		ret
	with e -> (cleanup (); raise e)

let build_universe ~repos ~ocaml_version ~base_packages ~direct_packages () =
	let available_packages = ref OpamPackage.Map.empty in

	let global_vars = Opam_metadata.init_variables () in

	let lookup_var package =
		let version = OpamPackage.version package in
		let name = OpamPackage.name package |> Name.to_string in
		Vars.(lookup {
			packages = (StringMap.from_list (
				[
					name, (Installed {
						path = None; (* not yet known *)
						version = Some version;
					});
					"ocaml", (Installed {
						path = None; (* not yet known *)
						version = Some ocaml_version;
					})
				] @ (base_packages |> List.map (fun name -> (name, Provided)))
			));
			prefix = None; (* not known *)
			self = name;
			vars = global_vars;
		}) in

	let add_package ~(package:OpamPackage.t) loaded =
		let lookup_var = lookup_var package in
		let available_filter = OPAM.available loaded.opam in
		let available =
			try package.name <> (Name.of_string "opam") && OpamFilter.eval_to_bool lookup_var available_filter
			with e -> (
				Printf.eprintf "Assuming package %s is unavailable due to error: %s\n" (OpamPackage.to_string package) (Printexc.to_string e);
				false
			)
		in
		if available then (
			available_packages := OpamPackage.Map.add package loaded !available_packages
		) else (
			let vars = OpamFilter.variables available_filter in
			let vars_str = String.concat "/" (List.map OpamVariable.Full.to_string vars) in
			Util.debug "  # Ignoring package %s (incompatible with %s)\n" (OpamPackage.to_string package) vars_str
		)
	in

	Repo.traverse ~repos (fun package ->
		let full_path = Repo.package_path package in
		let repository_expr = lazy (
			let digest = nix_digest_of_path (Repo.package_path package) in
			`Dir (Nix_expr.(`Call [
				`Id "repoPath";
				`Id "repo";
				`Attrs (AttrSet.build [
					"package", str package.path;
					"hash", str digest;
				])
			]))
		) in
		let opam = Opam_metadata.load_opam (Filename.concat full_path "opam") in
		let url = lazy (
			let open Opam_metadata in
			let urlfile = match OPAM.url opam with
				| Some _ as url -> url
				| None -> load_url (Filename.concat full_path "url")
			in
			(* TODO skip these packages instead of failing *)
			urlfile |> Option.map (fun urlfile ->
				try url urlfile
				with Unsupported_archive reason -> raise (
					Unsupported_archive (OpamPackage.to_string package.package ^ ": " ^ reason)
				)
			)
		) in
		add_package ~package:package.package { opam; url; repository_expr }
	);
	direct_packages |> List.iter (fun package ->
		let name = package.direct_name in
		let version = package.direct_version |> Option.default (Version.of_string "development") in
		add_package ~package:(OpamPackage.create name version)
			{
				opam = package.direct_opam;
				url = Lazy.from_val None; (* source is expected to be overridden *)
				repository_expr = Lazy.from_val (`File (Nix_expr.str package.direct_opam_relative));
			}
	);

	let available_packages = !available_packages in
	let base_packages = ("ocaml" :: base_packages)
		|> List.map (fun name -> OpamPackage.create (Name.of_string name) ocaml_version)
		|> OpamPackage.Set.of_list
	in
	let get_depends deptype_access =
		OpamPackage.Map.mapi (fun pkg { opam; _ } ->
			OpamFilter.partial_filter_formula (lookup_var pkg) (deptype_access opam)
		) available_packages
	in
	let conflicts =
		OpamPackage.Map.mapi (fun pkg { opam; _ } ->
			let conflicts = OPAM.conflicts opam in
			OpamFilter.filter_formula (lookup_var pkg) conflicts
		) available_packages
	in
	(available_packages, { OpamSolver.empty_universe with
		u_packages  = OpamPackage.Set.empty;
		u_action    = Install;
		u_installed = base_packages;
		u_base      = base_packages;
		u_available = available_packages |> OpamPackage.Map.bindings |> List.map fst |> OpamPackage.Set.of_list;
		u_depends   = get_depends OPAM.depends;
		u_depopts   = get_depends OPAM.depopts;
		u_conflicts = conflicts;
	})

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

let setup_repo ~update ~path ~commit : string =
	let open Util in
	let print = false in
	let git args = ["git"; "-C"; path ] @ args in
	FileUtil.mkdir ~parent:true (Filename.dirname path);
	let clone_repo () =
		Printf.eprintf "Cloning %s...\n" repo_url; flush stderr;
		rm_r path;
		Cmd.run_unit_exn [ "git"; "clone"; repo_url; path ]
	in
	let origin_head = "origin/HEAD" in
	let get_head_commit () =
		Cmd.run_output_exn ~print (git ["rev-parse"; "HEAD"]) |> String.trim in
	let fetch_into_head commit =
		(* TODO quiet: *)
		Cmd.run_unit_exn ~print (git ["fetch"; "--force"; repo_url]);
		Cmd.run_unit_exn ~print (git ["reset"; "--hard"; commit]);
		get_head_commit ()
	in
	(* TODO need to lock? ... *)
	let update_repo () =
		match commit with
			| Some commit ->
					(* only update if git lacks the given ref *)
					if Cmd.run_unit ~join:Cmd.join_success_bool ~print (git ["cat-file"; "-e"; commit]) then (
						Cmd.run_unit_exn ~print (git ["reset"; "--hard"; commit]);
						commit
					) else fetch_into_head commit
			| None ->
				if update
					then fetch_into_head origin_head
					else get_head_commit ()
	in
	if not (Sys.file_exists path) then clone_repo ();
	update_repo ()
;;

let setup_external_constraints
	~repo_commit ~detect_from ~ocaml_version ~opam_repo ~update : external_constraints =
	let remove_quotes s = s
		|> Str.global_replace (Str.regexp "\"") ""
		|> String.trim
	in
	let detect_nixpkgs_ocaml_version () =
		Util.debug "detecting current <nixpkgs> ocaml version\n";
		Cmd.run_output_opt ~print:false
			[ "nix-instantiate"; "--eval"; "--attr"; "ocaml.version"; "<nixpkgs>" ]
		|> Option.map remove_quotes
	in

	let detected_commit_and_ocaml_version = lazy (
		if Sys.file_exists detect_from then (
			Util.debug "detecting commit and version from %s\n" detect_from;
			let fullpath = if Filename.is_relative detect_from
				then Filename.concat (Unix.getcwd ()) detect_from
				else detect_from
			in
			Cmd.run_output_opt ~print:false
				[ "nix-instantiate"; "--eval"; "--expr" ; "with (import \"" ^ fullpath ^ "\" {}); \"${opam-commit} ${ocaml-version}\"" ]
				|> Option.map (fun str -> str
					|> remove_quotes
					|> String.split_on_char ' '
				) |> Option.bind (function
					| [ commit; version ] -> Some (commit, version)
					| other -> (
						Printf.eprintf "Can't parse commit and version: %s\n" (String.concat " " other);
						None
					)
				)
		) else None
	) in

	let ocaml_version = Version.of_string (match ocaml_version with
		| "" -> (
			match Lazy.force detected_commit_and_ocaml_version with
				| Some (_commit, version) -> (
					Printf.eprintf "Detected ocaml version %s\n" version;
					version
				)
				| None -> (
					Printf.eprintf "Using current <nixpkgs> ocaml version, pass --ocaml-version to override\n";
					detect_nixpkgs_ocaml_version () |> Option.or_failwith "Couldn't extract ocaml version from nixpkgs, pass --ocaml-version"
				)
		)
		| version -> version
	) in

	let repo_commit = match (repo_commit, update) with
		| (None, false) -> (
				(* If we're not updating and we haven't been given a commit, try to
				 * detect it from the exising `detect_from` *)
				match Lazy.force detected_commit_and_ocaml_version with
					| Some (commit, _) -> (
						Printf.eprintf "Detected opam-repository commit %s from %s\n" commit detect_from;
						Some commit
					)
					| None -> (
						Printf.eprintf "Unable to detect existing commit from %s\n" detect_from;
						None
					)
			)
		| (provided, _) -> provided
	in
	let repo_commit = setup_repo ~update ~path:opam_repo ~commit:repo_commit in
	(* TODO this could return a temp dir which we use to avoid needing a lock on the repo *)
	let repo_sha256 = nix_digest_of_git_repo opam_repo in
	{
		repo_commit;
		ocaml_version;
		repo_sha256;
	}
;;

let write_solution ~external_constraints ~available_packages ~cache ~base_packages ~universe solution dest =
	let new_packages = OpamSolver.new_packages solution in
	let () = match OpamPackage.Set.fold (fun pkg lst ->
		if OpamPackage.name pkg |> Name.to_string = "ocaml"
			then lst (* ignore newer ocaml versions, they're often fake *)
			else lst @ (newer_versions universe.u_available pkg)
	) new_packages [] with
		| [] -> ()
		| newer_versions ->
			Printf.eprintf "\nNOTE:\nThe following package versions are newer than the selected versions,\nbut were not selected due to version constraints:\n";
			newer_versions |> List.iter (fun pkg ->
				Printf.eprintf " - %s\n" (OpamPackage.to_string pkg)
			)
	in

	let deps = new Opam_metadata.dependency_map in
	let open Nix_expr in

	let selection = OpamPackage.Set.fold (fun pkg map ->
		let open Opam_metadata in
		let { opam; url; repository_expr } = OpamPackage.Map.find pkg available_packages in

		let expr : Nix_expr.t option = (
			(* TODO, obvs *)
			let offline = false in
			let ignore_broken_packages = ref false in
			let handle_error desc e =
				if !ignore_broken_packages then (
					prerr_endline ("Warn: " ^ desc); None
				) else raise e
			in
			try
				(* TODO simplify args *)
				Some (nix_of_opam ~cache ~offline ~deps ~pkg ~opam_src:(Lazy.force repository_expr) ~opam ~url:(Lazy.force url) ())
			with
			| Unsupported_archive desc as e -> handle_error ("Unsupported archive: " ^ desc) e
			| Invalid_package desc as e -> handle_error ("Invalid package: " ^ desc) e
			| Checksum_mismatch desc as e -> handle_error ("Checksum mismatch: " ^ desc) e
			| Not_cached desc as e -> handle_error ("Resource not cached: " ^ desc) e
			| Download.Download_failed url as e -> handle_error ("Download failed: " ^ url) e
		) in
		match expr with None -> map | Some expr ->
			AttrSet.add (OpamPackage.name pkg |> Name.to_string) expr map
	) new_packages AttrSet.empty in
	(* mark base packages as present *)
	let selection = List.fold_right (fun base -> AttrSet.add base (`Lit "true")) base_packages selection in

	let attrs = [
		"format-version", `Int 4;
		"opam-commit", `Id "opam-commit";
		"ocaml-version", str (external_constraints.ocaml_version |> Version.to_string);
		"selection", `Attrs selection
	] in

	let expr : Nix_expr.t = `Function (
		`Id "self",
		`Let_bindings (AttrSet.build [
			"lib", `Lit "self.lib";
			"pkgs", `Lit "self.pkgs";
			"selection", `Lit "self.selection";
			"repoPath", `Lit "self.repoPath";
			"opam-commit", str external_constraints.repo_commit;
			"repo", `Call [
				`Property (`Id "pkgs", "fetchFromGitHub");
				`Attrs (AttrSet.build [
					"owner", str repo_owner;
					"repo", str repo_name;
					"rev", `Id "opam-commit";
					"sha256", str external_constraints.repo_sha256;
				])
			];
		],
		`Attrs (AttrSet.build attrs);
	)) in
	let oc = open_out dest in
	Nix_expr.write oc expr;
	close_out oc;
	Printf.eprintf "Wrote %s\n" dest;
;;

let is_likely_path p =
	String.contains p '.' &&
	Str.string_match (Str.regexp ".*opam$") p 0

let main ~update_opam idx args =
	let dest = ref "opam-packages.nix" in
	let detect_from = ref "" in
	let ocaml_version = ref "" in
	let base_packages = ref "" in
	let repo_commit = ref "" in
	let opts = Arg.align [
		("--repo-commit", Arg.Set_string repo_commit, "Repository commit, default " ^
			(if update_opam then "update and use origin/HEAD" else "extract from DEST, otherwise current HEAD"));
		("--dest", Arg.Set_string dest, "Destination .nix file (default " ^ !dest ^ ")");
		("--from", Arg.Set_string detect_from, "Use instead of DEST as existing nix file (for commit / version detection)");
		("--ocaml-version", Arg.Set_string ocaml_version, "Target ocaml version, default extract from DEST, falling back to current nixpkgs.ocaml version");
		("--base-packages", Arg.Set_string base_packages, "Available base packages (comma-separated, not typically necessary)");
		("--verbose", Arg.Set Util._verbose, "Verbose");
		("-v", Arg.Set Util._verbose, "Verbose");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse_argv ~current:(ref idx) args opts add_package "opam2nix: usage...";

	let () =
		if Util.verbose () then
			OpamCoreConfig.update ~debug_level:2 ()
	in

	if !packages = [] then failwith "At least one package required";
	let (repo_packages, direct_packages) = List.partition (fun pkg ->
		not (is_likely_path pkg && Sys.file_exists pkg)
	) !packages in
	let dest = !dest in
	let detect_from = match !detect_from with "" -> dest | other -> other in

	let direct_packages = direct_packages |> List.map (fun p ->
		let open OPAM in
		let opam_filename = Filename.basename p in
		let opam = Opam_metadata.load_opam p in
		let name = opam.name |> Option.default_fn (fun () ->
			match String.split_on_char '.' opam_filename with
				| [ name; "opam" ] -> Name.of_string name
				| _ -> failwith ("Can't determine name of package in " ^ p)
		) in
		{
			direct_name = name;
			direct_version = opam.version;
			direct_opam = opam;
			direct_opam_relative = opam_filename;
		}
	) in

	let requested_packages : OpamFormula.atom list = repo_packages |> List.map (fun spec ->
		let relop_re = Str.regexp "[!<=>]+" in
		match Str.full_split relop_re spec with
			| [Str.Text name; Str.Delim relop; Str.Text ver] ->
				let relop = OpamLexer.relop relop in
				let ver = OpamPackage.Version.of_string ver in
				(OpamPackage.Name.of_string name, Some (relop, ver))
			| [Str.Text name] -> (OpamPackage.Name.of_string name, None)
			| _ -> failwith ("Invalid version spec: " ^ spec)
	) in
	let requested_packages = requested_packages @ (direct_packages |> List.map (fun package ->
		(package.direct_name, package.direct_version |> Option.map (fun v -> `Eq, v))
	)) in
	let package_names : OpamPackage.Name.t list = requested_packages |> List.map (fun (name, _) -> name) in

	let config_base = Filename.concat (Unix.getenv "HOME") ".cache/opam2nix" in
	let digest_map = Filename.concat config_base "digest.json" in
	let opam_repo = Filename.concat config_base "opam-repository" in

	let repo_commit = if !repo_commit = "" then None else Some !repo_commit in
	let external_constraints = setup_external_constraints
		~repo_commit
		~detect_from
		~opam_repo
		~update:update_opam
		~ocaml_version:!ocaml_version in

	(* Note: seems to be unnecessary as of opam 2 *)
	let base_packages = !base_packages |> Str.split (Str.regexp ",") in

	Printf.eprintf "Loading repository...\n"; flush stderr;
	let (available_packages, universe) = build_universe
		~repos:[opam_repo] (* TODO custom repos? *)
		~base_packages
		~ocaml_version:external_constraints.ocaml_version
		~direct_packages
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

	let cache = (
		FileUtil.mkdir ~parent:true (Filename.dirname digest_map);
		Util.debug "Using digest mapping at %s\n" digest_map;
		try
			Digest_cache.try_load digest_map
		with e -> (
			Printf.eprintf "Error loading %s, you may need to delete or fix it manually\n" digest_map;
			raise e
		)
	) in

	Printf.eprintf "Solving...\n";
	flush stderr;
	let () = OpamSolverConfig.init () in
	let () = (match OpamSolver.resolve universe ~orphans:OpamPackage.Set.empty request with
		| Success solution ->
				OpamSolver.print_solution
					~messages:(fun pkg -> [OpamPackage.to_string pkg])
					~append:(fun _nv -> "")
					~requested:(package_names |> OpamPackage.Name.Set.of_list)
					~reinstall:OpamPackage.Set.empty
					solution;
				write_solution
					~external_constraints ~available_packages ~cache ~base_packages ~universe
					solution dest
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
