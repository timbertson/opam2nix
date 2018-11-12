open Util

let (%) f g x = f (g x)

type repo_type = [ `Nix | `Opam ]

type version = Version of string

module Package_set = Set.Make (struct
	type t = string * version
	let compare (a1, a2) (b1, b2) =
		let r0 = Pervasives.compare a1 b1 in
		if r0 <> 0 then r0 else Pervasives.compare a2 b2
end)

type version_filter_component = [ `All | `Some of int ]

type version_filter =
	[ `Filter of (version list -> version list)
	| `Exact of version
	]

type version_selection =
	[ `All
	| `Filter of (version list -> version list)
	| `Exact of version
	]

type package_selection =
	[ `All
	| `Filtered of version_filter
	| `Package of (string * version_selection)
	]

let opam_version_of = function
	| Version v -> OpamPackage.Version.of_string v

let string_of_version = function Version v -> v

let path_of_version (repo_type:repo_type) = match repo_type with
	| `Nix -> encode_nix_safe_path % string_of_version
	| `Opam -> string_of_version

let version_of_filename (repo_type:repo_type) filename =
	match repo_type with
		| `Opam -> Version filename
		| `Nix -> Version (decode_nix_safe_path filename)

let decreasing_version_order versions =
	let compare a b =
		(* Note: we invert this because we want a descending list *)
		OpamPackage.Version.compare
			(opam_version_of b)
			(opam_version_of a)
	in
	versions |> List.sort compare

let latest_version versions = List.hd (decreasing_version_order versions)

type package_selections = package_selection list

let version_filter filter_spec : (version list -> version list) = (fun versions ->
	let dot = Str.regexp "\\." in
	let digits = Str.regexp "^[0-9]+$" in
	let semantic_versions = decreasing_version_order versions |> List.map (fun version ->
		let parts = match version with Version v -> Str.split dot v in
		(version, parts)
	) in

	let rec filter num_versions versions = (
		let num_this, num_versions = match num_versions with
			| [] -> (`Some 1, []) (* assume 1 version for any part left unspecified *)
			| num_this :: num_versions -> (num_this, num_versions)
		in

		(* special-case: include all versions with a non-digit terminal.
		 * This is because typically non-digit patchlevels like +system are variants
		 * rather than versions, and should all be included *)
		let is_nondigit_terminal = fun (_version, parts) -> (
			let is_digit s = Str.string_match digits s 0 in
			match parts with
				| [p] when not (is_digit p) -> true
				| _ -> false
		) in
		let (nondigit_terminals, versions) = versions |> List.partition is_nondigit_terminal in

		let groups = versions |> group_by (fun (_version, parts) -> head_opt parts |> Option.default "0") in

		let taken_groups = match num_this with
			| `Some num -> groups |> take num
			| `All -> groups
		in
		(* Printf.eprintf "taking (%d/%d) groups, with keys: %s\n" *)
		(* 	num_this *)
		(* 	(List.length groups) *)
		(* 	(taken_groups *)
		(* 		|> List.map fst *)
		(* 		|> String.concat ";") *)
		(* ; *)
		(* taken_groups |> List.iter (fun (key, versions) -> *)
		(* 	versions |> List.iter (fun (version,parts) -> *)
		(* 		Printf.eprintf " %s: %s (%s)\n" *)
		(* 			(key) *)
		(* 			(string_of_version version) *)
		(* 			(parts |> String.concat ".") *)
		(* 	) *)
		(* ); *)

		let filtered = taken_groups |> List.map (fun (_, versions) ->
			let sub_versions = versions |> List.map (fun (version, parts) -> (version, tail parts)) in
			if (List.for_all (fun (_, parts) -> parts = []) sub_versions)
				then take 1 sub_versions (* no sub filtering required *)
				else filter num_versions sub_versions
		) |> List.concat in
		nondigit_terminals @ filtered
	) in

	filter filter_spec semantic_versions |> List.map fst
)

let parse_version_filter s =
	let parse_number s =
		try int_of_string s
		with _ -> failwith ("Invalid number in version filter: "^s)
	in
	let parse_version_filter_component = function
		| "*" -> `All
		| n -> `Some (parse_number n)
	in
	let parts = Str.split (Str.regexp "\\.") s |> List.map parse_version_filter_component in
	(* Printf.eprintf "version filter: [%s]\n" (String.concat "," (List.map string_of_int parts)); *)
	`Filter (version_filter parts)

let parse_package_spec spec =
	let sep = Str.regexp "@" in
	match Str.bounded_split sep spec 2 with
		| [package] | [package; "*"] -> (package, `All)
		| [package; version] ->
			if (Str.string_match (Str.regexp "\\*") version 0) then (
				let spec = String.sub version 1 ((String.length version) - 1) in
				(package, parse_version_filter spec)
			) else (
				(package, `Exact (Version version))
			)
		| _ -> failwith ("Invalid package specifier: " ^ spec)

let traverse repo_type ~repos ~(packages:package_selections) (emit: string -> version -> string -> unit) =
	let version_sep = "." in
	let version_join package version =
		let version_path = path_of_version repo_type version in
		match repo_type with
			| `Nix -> version_path
			| `Opam -> package ^ version_sep ^ version_path in
	let seen = ref Package_set.empty in
	let emit package version path =
		let id = (package, version) in
		if Package_set.mem id !seen then
			Printf.eprintf "Skipping %s (already loaded %s.%s)\n" path package (string_of_version version)
		else begin
			seen := Package_set.add id !seen;
			debug "Processing package %s\n" path;
			try
				emit package version path
			with e -> (
				Printf.eprintf "Error raised while processing %s:\n" path;
				raise e
			)
		end
	in

	repos |> List.iter (fun repo ->
		let pkgroot = match repo_type with `Nix -> repo | `Opam -> Filename.concat repo "packages" in

		let process_package package (version:version_selection) =
			debug "processing package %s\n" package;
			let package_base = Filename.concat pkgroot package in
			let list_versions () =
				debug "listing %s\n" package_base;
				let dirs = list_dirs package_base in
				let version_dirs = match repo_type with
					| `Nix -> dirs
					| `Opam ->
						let prefix = package ^ version_sep in
						dirs |> filter_map (fun ent ->
							match ent |> without_leading prefix with
								| None -> Printf.eprintf "Skipping non-package directory %s ()\n" ent; None
								| x -> x
						)
				in
				List.map (version_of_filename repo_type) version_dirs
			in

			let versions = match version with
				| `All ->
						debug "processing all versions\n";
						list_versions ()
				| `Exact version ->
						debug "selecting only version %s\n" (string_of_version version);
						[version]
				| `Filter fn ->
						let all_versions = list_versions () in
						let filtered = fn all_versions in
						debug "filtered to %d versions (of %d total)\n"
							(List.length filtered) (List.length all_versions);
						filtered
			in
			versions |> List.iter (fun version ->
				let path = Filename.concat package_base (version_join package version) in
				emit package version path
			)
		in

		let list_packages () = list_dirs pkgroot in
		packages |> List.iter (function
			| `All ->
				list_packages ()
				|> List.iter (fun pkg -> process_package pkg `All)

			| `Filtered filter ->
				let filter = (filter :> version_selection) in
				list_packages ()
				|> List.iter (fun pkg -> process_package pkg filter)

			| `Package (name, filter) -> process_package name filter
		)
	)

let traverse_versions (repo_type:[`Nix]) ~root emit =
	let dirs = list_dirs root in
	dirs |> List.iter (fun pkg ->
		let pkg_path = Filename.concat root pkg in
		let versions = list_dirs pkg_path |> List.map (version_of_filename (repo_type:>repo_type)) in
		match versions with
			| [] -> ()
			| versions -> emit pkg (decreasing_version_order versions) pkg_path
	)

