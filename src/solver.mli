type error = [
	Opam_metadata.unsupported_archive
	| `unavailable of string
]

type external_constraints = {
	ocaml_version: OpamPackage.Version.t;
	repos: Repo.t list;
}

type universe = {
	lookup_var : OpamPackage.t -> OpamVariable.Full.t -> OpamVariable.variable_contents option;
	repos : Repo.t list;
	packages : (Repo.loaded_package, error) result OpamPackage.Map.t ref;
	constrained_versions : OpamPackage.Version.t OpamPackage.Name.Map.t;
}

val is_available
	: lookup_var:(Opam_metadata.PackageMap.key -> OpamFilter.env)
	-> opam:OpamFile.OPAM.t
	-> package:Opam_metadata.PackageMap.key
	-> (unit, [> `unavailable of string ]) result

val build_universe
	: external_constraints:external_constraints
	-> base_packages:OpamPackage.Name.t list
	-> constrained_versions:(OpamPackage.Name.t * OpamPackage.Version.t) list
	-> direct_definitions:Repo.direct_package list
	-> unit
	-> universe

type diagnostics

val diagnostics : ?verbose:bool -> diagnostics -> string

type selections

val packages_of_result : selections -> Opam_metadata.PackageMap.key list

val solve : universe -> OpamPackage.Name.t list -> (selections, diagnostics) result
