val load_opam : string -> OpamFile.OPAM.t

type opam_src = [ `Dir of Nix_expr.t | `File of Nix_expr.t ]

type url = [
	| `http of string * (Digest_cache.opam_digest list)
]

type unsupported_archive = [ `unsupported_archive of string ]

val url : OpamFile.URL.t -> (url, [> unsupported_archive ]) result

val string_of_unsupported_archive : unsupported_archive -> string

val string_of_url : url -> string

val load_opam_string : string -> OpamFile.OPAM.t

val nix_of_url : cache:Digest_cache.t -> url -> (Nix_expr.t, Digest_cache.error) result Lwt.t

type dependency =
	| NixDependency of string
	| SimpleOpamDependency of string
	| ExternalDependencies of (OpamSysPkg.Set.t * OpamTypes.filter) list
	| PackageDependencies of OpamTypes.filtered_formula

type importance = Required | Optional
type requirement = importance * dependency

class dependency_map :
  object
    method add_dep : OpamPackage.t -> requirement -> unit
    method init_package : OpamPackage.t -> unit
    method to_string : string
  end

val nix_of_opam
	: pkg:OpamPackage.t
	-> deps:< init_package : OpamPackage.t -> unit; .. >
	-> opam_src:opam_src
	-> opam:OpamFile.OPAM.t
	-> src:Nix_expr.t option
	-> url:url option
	-> unit
	-> Nix_expr.t
