open Util
module Name = OpamPackage.Name
module Version = OpamPackage.Version
module OPAM = OpamFile.OPAM
open Lwt.Infix
open Repo

let unique_file_counter =
	let state = ref 0 in
	fun () -> (
		let ret = !state in
		incr state;
		ret
	)

(* TODO return result? *)
let nix_digest_of_git_repo p =
	let print = false in
	let tempname = Printf.sprintf "opam2nix-%d-%d" (Unix.getpid ()) (unique_file_counter ()) in
	let tempdir = Filename.concat (Filename.get_temp_dir_name ()) tempname in
	Unix.mkdir tempdir 0o700;
	let cleanup () = rm_r tempdir; Lwt.return_unit in
	Lwt.finalize (fun () ->
		let (r,w) = Unix.pipe ~cloexec:true () in
		let open Cmd in
		run_exn (exec_none ~stdin:(`FD_move r)) ~print ~block:(fun _proc ->
			run_unit_exn (exec_none ~stdout:(`FD_move w)) ~print
				[ "git"; "-C"; p; "archive"; "HEAD" ]
		) [ "tar"; "x"; "-C"; tempdir ] >>= (fun () ->
			nix_digest_of_path tempdir
		)
	) cleanup

let newer_versions available pkg =
	let newer a b =
		(* is a newer than b? *)
		OpamPackage.Version.compare (OpamPackage.version a) (OpamPackage.version b) > 0
	in
	available
		|> List.filter (fun avail ->
			OpamPackage.name avail == OpamPackage.name pkg && newer avail pkg
		)
		|> List.sort (fun a b ->
			OpamPackage.Version.compare (OpamPackage.version a) (OpamPackage.version b)
		)

let setup_repo ~cache ~repos_base ~key spec : Repo.t Lwt.t = (
	let open Util in
	let repo_path = Filename.concat repos_base key in
	let repo_url = git_url spec in
	let clone_repo () =
		Printf.eprintf "Cloning %s...\n" repo_url; flush stderr;
		rm_r repo_path;
		Cmd.run_unit_exn Cmd.exec_none [ "git"; "clone"; repo_url; repo_path ]
	in
	let print = false in
	let git args = ["git"; "-C"; repo_path ] @ args in
	let ref = "fetched" in
	let resolve_commit rev = Cmd.run_output_exn ~print (git ["rev-parse"; rev]) |> Lwt.map String.trim in
	let run_devnull = Cmd.run_unit_exn (Cmd.exec_none ~stdout:`Dev_null ~stderr:`Dev_null) ~print in
	let fetch () =
		Printf.eprintf "Fetching...\n";
		run_devnull (git ["fetch"; "--force"; repo_url; "HEAD:refs/heads/" ^ ref]) in
	let reset_hard commit = run_devnull (git ["reset"; "--hard"; commit]) in

	(* TODO need to lock? ... *)
	let update_repo () =
		(match spec.spec_commit with
			| Some commit ->
				(* only fetch if git lacks the given ref *)
				Cmd.run_unit (Cmd.exec_none) ~join:Cmd.join_success_bool ~print (git ["cat-file"; "-e"; commit]) >>= (fun has_commit ->
					if not has_commit then fetch () else Lwt.return_unit
				) |> Lwt.map (fun () -> commit)
			| None -> fetch () |> Lwt.map (fun () -> ref)
		) >>= fun commit ->
		reset_hard commit >>= fun () ->
		resolve_commit commit
	in

	let get_digest ~commit () =
		(* TODO this could return a temp dir which we use to avoid needing a lock on the repo *)
		Printf.eprintf "Importing %s %s into nix store...\n" key commit;
		Digest_cache.add_custom cache ~keys:["git:" ^ commit] (fun () ->
			nix_digest_of_git_repo repo_path |> Lwt.map Result.ok
		) |> Lwt.map (Result.get_exn Digest_cache.string_of_error)
	in

	let setup () = if Sys.file_exists repo_path then (
		Lwt.return_unit
	) else (
		FileUtil.mkdir ~parent:true (Filename.dirname repo_path);
		clone_repo ()
	) in

	(
		setup ()
		>>= update_repo
		|> Lwt.map (fun commit -> Repo.{
			spec;
			repo_key = key;
			repo_path;
			repo_commit = commit;
			repo_digest = get_digest ~commit ();
		})
	)
)
;;

let setup_external_constraints
	~repos_base ~repos ~detect_from ~ocaml_version ~cache : Solver.external_constraints Lwt.t =
	let remove_quotes s = s
		|> Str.global_replace (Str.regexp "\"") ""
		|> String.trim
	in
	let detect_nixpkgs_ocaml_version () =
		Util.debug "detecting current <nixpkgs> ocaml version\n";
		Cmd.run_output_opt ~print:false
			[ "nix-instantiate"; "--eval"; "--attr"; "ocaml.version"; "<nixpkgs>" ]
		|> Lwt.map (Option.map remove_quotes)
	in

	let detect_ocaml_version () =
		if Sys.file_exists detect_from then (
			Util.debug "detecting ocaml version from %s\n" detect_from;
			let fullpath = if Filename.is_relative detect_from
				then Filename.concat (Unix.getcwd ()) detect_from
				else detect_from
			in
			Cmd.run_output_opt ~print:false
				[ "nix-instantiate"; "--eval"; "--expr" ; "with (import \"" ^ fullpath ^ "\" {}); ocaml-version" ]
				|> Lwt.map (Option.map remove_quotes)
		) else Lwt.return_none
	in

	let ocaml_version =
		(match ocaml_version with
			| "" -> (
				detect_ocaml_version () >>= function
					| Some version -> (
						Printf.eprintf "Detected ocaml version %s\n" version;
						Lwt.return version
					)
					| None -> (
						Printf.eprintf "Using current <nixpkgs> ocaml version, pass --ocaml-version to override\n";
						detect_nixpkgs_ocaml_version () |> Lwt.map (Option.or_failwith "Couldn't extract ocaml version from nixpkgs, pass --ocaml-version")
					)
			)
			| version -> Lwt.return version
		) |> Lwt.map Version.of_string
	in

	let resolved_repos = StringMap.bindings repos |> Lwt_list.map_p (fun (key, spec) ->
		setup_repo ~cache ~key ~repos_base spec
	) in

	Lwt.both ocaml_version resolved_repos |> Lwt.map (fun (ocaml_version, repos) ->
		Solver.{ ocaml_version; repos; }
	)
;;

let write_solution ~external_constraints ~cache  ~universe installed dest =
	let open Solver in
	let available_packages = !(universe.packages)
		|> OpamPackage.Map.bindings
		|> List.filter_map (fun (key, value) -> match value with
			| Ok pkg -> Some (key, pkg)
			| Error _ -> None
		)
	|> OpamPackage.Map.of_list in

	let () = match installed |> List.concat_map (fun pkg ->
		if OpamPackage.name pkg |> Name.to_string = "ocaml"
			then [] (* ignore newer ocaml versions, they're often fake *)
			else newer_versions (OpamPackage.Map.keys available_packages) pkg
	) with
		| [] -> ()
		| newer_versions ->
			Printf.eprintf "\nNOTE:\nThe following package versions are newer than the selected versions,\nbut were not selected due to version constraints:\n";
			newer_versions |> List.iter (fun pkg ->
				Printf.eprintf " - %s\n" (OpamPackage.to_string pkg)
			)
	in

	let deps = new Opam_metadata.dependency_map in
	let open Nix_expr in

	(* download all new packages (Digest_cache usage will automatically throttle *)
	let new_packages = installed |> Lwt_list.map_p (fun pkg ->
		let open Opam_metadata in
		let { loaded_opam; loaded_url; src_expr; repository_expr } = OpamPackage.Map.find pkg available_packages in
		Lwt.both (src_expr cache) (repository_expr ()) |> Lwt.map (fun (src, repository_expr) ->
			let src = src |> Result.get_exn (fun e ->
				let url = Option.to_string Opam_metadata.string_of_url loaded_url in
				Printf.sprintf "%s (%s)" (Digest_cache.string_of_error e) url
			) in
			(OpamPackage.name pkg |> Name.to_string,
				nix_of_opam ~deps ~pkg ~opam:loaded_opam ~url:loaded_url
					~src ~opam_src:repository_expr ())
		)
	) |> Lwt_main.run in

	(* add downloaded packages *)
	let selection = List.fold_right (fun (name, expr) map ->
		AttrSet.add name expr map
	) new_packages AttrSet.empty in

	let attrs = [
		`Expr ("format-version", `Int 4);
		`Inherit (None, ["repos"]);
		`Expr ("ocaml-version", str (external_constraints.ocaml_version |> Version.to_string));
		`Expr ("selection", `Attrs selection)
	] in

	let sha256 digest = Lwt_main.run digest |> fun (`sha256 x) -> x in

	let repo_attrsets = external_constraints.repos |> List.map (fun repo ->
		let open Repo in
		let spec = repo.spec in
		(repo.repo_key, `Rec_attrs (AttrSet.build [
			"fetch", `Attrs (AttrSet.build [
				"owner", str spec.github_owner;
				"repo", str spec.github_name;
				"rev", str repo.repo_commit;
				"sha256", str (sha256 repo.repo_digest);
			]);
			"src", `Call [
				`Property (`Id "pkgs", "fetchFromGitHub");
				`Id "fetch";
			];
		]))
	) in

	let expr : Nix_expr.t = `Function (
		`Id "self",
		`Let_bindings (AttrSet.build [
			"lib", `Lit "self.lib";
			"pkgs", `Lit "self.pkgs";
			"selection", `Lit "self.selection";
			"repoPath", `Lit "self.repoPath";
			"repos", `Attrs (AttrSet.build repo_attrsets);
		],
		`Attrs attrs;
	)) in
	Lwt_main.run (Digest_cache.save cache);
	let oc = open_out dest in
	(* TODO write to temp file and rename *)
	Printf.fprintf oc "### This file is generated by opam2nix.\n\n";
	Nix_expr.write oc expr;
	close_out oc;
	Printf.eprintf "Wrote %s\n" dest;
;;

let is_likely_path p =
	String.contains p '.' &&
	Str.string_match (Str.regexp ".*opam$") p 0
	
let main idx args =
	let dest = ref "opam-selection.nix" in
	let detect_from = ref "" in
	let ocaml_version = ref "" in
	let base_packages = ref "" in
	let repo_specs = ref (StringMap.singleton "opam-repository" {
		github_owner = "ocaml";
		github_name = "opam-repository";
		spec_commit = (try Some (Unix.getenv "OPAM_REPO_COMMIT") with Not_found -> None);
	}) in

	let parse_repo ~commit repo = match repo |> String.split_on_char '/' with
		| [github_owner; github_name] -> { github_owner; github_name; spec_commit = commit; }
		| _ -> failwith ("Can't parse github repo from: " ^ repo)
	in

	let set_repo key repo =
		let spec = match repo |> String.split_on_char '#' with
			| [repo] -> parse_repo ~commit:None repo
			| [repo; commit] -> parse_repo ~commit:(Some commit) repo
			| _ -> failwith ("Repo contains multiple `#` characters: " ^ repo)
		in
		let op = if StringMap.mem key !repo_specs then "Replacing" else "Adding" in
		Printf.eprintf "%s repository: %s ...\n" op key;
		repo_specs := !repo_specs |> StringMap.add key spec
	in

	let direct_definitions = ref [] in
	let opts = Arg.align [
		("--repo-commit", Arg.String Obj.magic, "COMMIT opam-repository commit, default will fetch and use origin/HEAD");
		("--repo",
			(let key = ref "" in
			Arg.Tuple [
				Arg.Set_string key;
				Arg.String (fun url -> set_repo !key url)
		]), "<owner>/<name>[#commit] Add additional repo (use key `opam-packages` to override the default)");
		("--dest", Arg.Set_string dest, "DEST Destination .nix file (default " ^ !dest ^ ")");
		("--from", Arg.Set_string detect_from, "NIX_FILE Use instead of DEST as existing nix file (for commit / version detection)");
		("--ocaml-version", Arg.Set_string ocaml_version, "VERSION Target ocaml version, default extract from DEST, falling back to current nixpkgs.ocaml version");
		("--base-packages", Arg.Set_string base_packages, "PACKAGES Available base packages (comma-separated, not typically necessary)");
		("--define", Arg.String (fun x -> direct_definitions := x :: !direct_definitions), "PACKAGE Define an .opam package without necessarily installing it");
		("--verbose", Arg.Set Util._verbose, " Verbose");
		("-v", Arg.Set Util._verbose, " Verbose");
	]; in
	let packages = ref [] in
	let add_package x = packages := x :: !packages in
	Arg.parse_argv ~current:(ref idx) args opts add_package "usage: opam2nix [OPTIONS] package [package...]";

	let () =
		if Util.verbose () then
			OpamCoreConfig.update ~debug_level:2 ()
	in

	if !packages = [] then failwith "At least one package required";
	let (direct_requests, repo_packages) = List.partition (fun pkg ->
		is_likely_path pkg && (
			let file_exists = Sys.file_exists pkg in
			if not file_exists then
				Printf.eprintf "WARN: %s looks like a path but does not exist on disk\n" pkg;
			file_exists
		)
	) !packages in
	let dest = !dest in
	let detect_from = match !detect_from with "" -> dest | other -> other in

	let load_direct = fun path ->
		let open OPAM in
		let opam_filename = Filename.basename path in
		let opam = Opam_metadata.load_opam path in
		let name = opam.name |> Option.default_fn (fun () ->
			match String.split_on_char '.' opam_filename with
				| [ name; "opam" ] -> Name.of_string name
				| _ -> failwith ("Can't determine name of package in " ^ path)
		) in
		{
			direct_name = name;
			direct_version = opam.version;
			direct_opam = opam;
			direct_opam_relative = opam_filename;
		}
	in

	let direct_requests = direct_requests |> List.map load_direct in
	let direct_definitions = direct_requests @ (!direct_definitions |> List.map load_direct) in

	let requested_packages = repo_packages |> List.map (fun spec ->
		let relop_re = Str.regexp "[!<=>]+" in
		Util.debug "Parsing spec %s\n" spec;
		match Str.full_split relop_re spec with
			| [Str.Text name; Str.Delim relop; Str.Text ver] ->
				(* As of 20200906, we only support explicit versioning in cmdline specs *)
				if String.equal relop "=" then (
					(Name.of_string name, Some (OpamPackage.Version.of_string ver))
				) else failwith ("Unsupported version operator: " ^ relop)
			| [Str.Text name] -> (OpamPackage.Name.of_string name, None)
			| _ -> failwith ("Invalid version spec: " ^ spec)
	) in

	let config_base =
		Filename.concat (
			match Sys.getenv_opt "XDG_CACHE_HOME" with
			| None -> Filename.concat (Unix.getenv "HOME") ".cache"
			| Some cache -> cache
		) "opam2nix"
	in
    let digest_map = Filename.concat config_base "digest.json" in
	let repos_base = Filename.concat config_base "repos" in
	let repo_specs = !repo_specs in

	(* Note: seems to be unnecessary as of opam 2 *)
	let base_packages = !base_packages |> Str.split (Str.regexp ",") |> List.map Name.of_string in

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

	let external_constraints = Lwt_main.run (setup_external_constraints
		~repos_base
		~repos:repo_specs
		~detect_from
		~cache
		~ocaml_version:!ocaml_version) in

	let requested_packages = requested_packages @ (direct_requests |> List.map (fun package ->
		(package.direct_name, package.direct_version)
	)) @ [(Name.of_string "ocaml-base-compiler", Some external_constraints.ocaml_version)] in

	let package_names : OpamPackage.Name.t list = requested_packages |> List.map fst in
	let constrained_versions = requested_packages |> List.filter_map (fun (name, version) ->
		version |> Option.map (fun version -> (name, version))
	) in

	let universe = Solver.build_universe
		~external_constraints
		~base_packages
		~constrained_versions
		~direct_definitions () in
		
	Printf.eprintf "Solving...\n"; flush stderr;
	flush stderr;

	let () = (match Solver.solve universe package_names with
		| Error e -> (
			print_endline (Solver.diagnostics e);
			exit 1
		)
		| Ok solution ->
			let installed = Solver.packages_of_result solution in
			Printf.eprintf "Selected packages:\n";
			installed |> List.iter (fun pkg -> Printf.eprintf "- %s\n" (OpamPackage.to_string pkg));
			write_solution ~external_constraints ~universe ~cache installed dest
	) in
	()
