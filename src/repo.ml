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

let parse_package_spec spec =
	let sep = Str.regexp "@" in
	match Str.split sep spec with
		| [package] -> (package, `All)
		| [package; version] -> (package, `Exact (Version version))
		| _ -> failwith ("Invalid package specifier: " ^ spec)

let traverse repo_type ~repos ~(packages:package_selections) ?verbose (emit: string -> version -> string -> unit) =
	let verbose = Option.default false verbose in
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
			if verbose then Printf.eprintf "Processing package %s\n" path;
			emit package version path
		end
	in

	repos |> List.iter (fun repo ->
		let pkgroot = Filename.concat repo "packages" in

		let process_package package (version:version_selection) =
			let package_base = Filename.concat pkgroot package in
			let list_versions () =
				if verbose then Printf.eprintf "listing %s\n" package_base;
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
				| `All -> list_versions ()
				| `Exact version -> [version]
				| `Filter fn -> fn (list_versions ())
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

let version_filter num_latest : (version list -> version list) = (fun versions ->
	let dot = Str.regexp "\\." in
	let digits = Str.regexp "^[0-9]+$" in
	let keep = ref [] in
	decreasing_version_order versions |> List.iter (fun version ->
		let major_minor = function Version v -> (
			let parts = Str.split dot v |> List.rev in
			match parts with
				| [] -> None
				| patch::parts ->
					if Str.string_match digits patch 0
						then Some (List.rev parts)
						(* non-digit patchlevel usually implies something weird like +system. Include it. *)
						else None
		) in
		(* Printf.eprintf "saw %s with base_version = %s; keep = %s\n" version (String.concat "." base_version) (String.concat ", " !keep); *)
		let duplicate : version option = major_minor version |> Option.bind (fun base_version ->
			try
				let predicate = fun candidate -> major_minor candidate = Some base_version in
				Some (List.find predicate !keep)
			with Not_found ->
				None
		) in
		match duplicate with
			| None -> keep := version :: !keep
			| Some _ -> ()
	);
	(* Printf.eprintf "keep is now: %s\n" (String.concat ", " !keep); *)
	!keep |> List.rev |> take num_latest
)

