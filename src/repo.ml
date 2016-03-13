open Util

let decreasing_version_order versions =
	let compare a b =
		(* Note: we invert this because we want a descending list *)
		OpamVersion.compare (OpamVersion.of_string b) (OpamVersion.of_string a)
	in
	versions |> List.sort compare

let latest_version versions = List.hd (decreasing_version_order versions)

type repo_type = [ `Nix | `Opam ]

module String_tuple_set = Set.Make (struct
	type t = string * string
	let compare (a1, a2) (b1, b2) =
		let r0 = Pervasives.compare a1 b1 in
		if r0 <> 0 then r0 else Pervasives.compare a2 b2
end)

type version_filter =
	[ `Filter of (string list -> string list)
	| `Exact of string
	]

type version_selection =
	[ `All
	| `Filter of (string list -> string list)
	| `Exact of string
	]

type package_selection =
	[ `All
	| `Filtered of version_filter
	| `Package of (string * version_selection)
	]

type package_selections = package_selection list

let parse_package_spec spec =
	let sep = Str.regexp "@" in
	match Str.split sep spec with
		| [package] -> (package, `All)
		| [package; version] -> (package, `Exact version)
		| _ -> failwith ("Invalid package specifier: " ^ spec)

let traverse repo_type ~repos ~(packages:package_selections) ?verbose emit =
	let verbose = Option.default false verbose in
	let version_sep = "." in
	let version_join = match repo_type with
		| `Nix -> fun package version -> version
		| `Opam -> fun package version -> package ^ version_sep ^ version in
	let seen = ref String_tuple_set.empty in
	let emit package version path =
		let id = (package, version) in
		if String_tuple_set.mem id !seen then
			Printf.eprintf "Skipping %s (already loaded %s.%s)\n" path package version
		else begin
			seen := String_tuple_set.add id !seen;
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
				match repo_type with
					| `Nix -> dirs
					| `Opam ->
						let prefix = package ^ version_sep in
						dirs |> filter_map (fun ent ->
							match ent |> without_leading prefix with
								| None -> Printf.eprintf "Skipping non-package directory %s ()\n" ent; None
								| x -> x
						)
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


let traverse_versions ~root emit =
	let dirs = list_dirs root in
	dirs |> List.iter (fun pkg ->
		let pkg_path = Filename.concat root pkg in
		let versions = list_dirs pkg_path in
		match versions with
			| [] -> ()
			| versions -> emit pkg (decreasing_version_order versions) pkg_path
	)

let version_filter num_latest = (fun versions ->
	let dot = Str.regexp "\\." in
	let keep = ref [] in
	decreasing_version_order versions |> List.iter (fun version ->
		let major_minor v =
			let parts = Str.split dot v |> List.rev in
			match parts with
				| [] -> []
				| patch::parts -> List.rev parts
		in
		let base_version = major_minor version in
		(* Printf.eprintf "saw %s with base_version = %s; keep = %s\n" version (String.concat "." base_version) (String.concat ", " !keep); *)
		try
			let predicate = fun candidate -> major_minor candidate = base_version in
			let _:string = List.find predicate !keep in ()
		with Not_found -> begin
			keep := version :: !keep
		end
	);
	(* Printf.eprintf "keep is now: %s\n" (String.concat ", " !keep); *)
	!keep |> List.rev |> take num_latest
)

