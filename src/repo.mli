type spec = {
	github_owner: string;
	github_name: string;
	spec_commit: string option;
}

type t = {
	repo_key: string;
	spec: spec;
	(* TODO: OpamFilename.Dir.t? *)
	repo_path: string;
	repo_commit: string;
	(* returned as Lwt.t because it's needed lazily much later than commit and version *)
	repo_digest: Digest_cache.nix_digest Lwt.t;
}

type package = {
	repo: t;
	rel_path: string;
	package: OpamPackage.t;
	opam: OpamFile.OPAM.t;
	url: OpamFile.URL.t option;
}

(** a direct package is passed on the commandline, and is not from any repository *)
type direct_package = {
	direct_opam_relative: string;
	direct_opam: OpamFile.OPAM.t;
	direct_name: OpamPackage.Name.t;
	direct_version: OpamPackage.Version.t option;
}

(** low level pkg returned by lookup, which is all Extract module needs *)
type lookup_result = {
	p_package: OpamPackage.t;
	p_opam: OpamFile.OPAM.t;
	p_url: OpamFile.URL.t option;
}

(** Loaded package, either from an opam repo or direct package supplied on the commandline *)
type loaded_package = {
	loaded_opam: OpamFile.OPAM.t;
	repository_expr: unit -> Opam_metadata.opam_src Lwt.t;
	src_expr: Digest_cache.t -> (Nix_expr.t option, Digest_cache.error) Result.t Lwt.t;
	loaded_url: Opam_metadata.url option;
}

val load_url : string -> Opam_metadata.URL.t option

val lookup_package_versions : string -> string -> lookup_result list

val nix_digest_of_path : string -> [> `sha256 of string ] Lwt.t

val lookup : string -> OpamPackage.t -> lookup_result option

val full_path : package -> string

val list_package : t -> string -> package list

val git_url : spec -> string
