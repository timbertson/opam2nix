type key = string

type opam_digest = OpamHash.t

type t

type nix_digest = [
	| `sha256 of string
]

type checksum_mismatch = [ `checksum_mismatch of string ]

type unknown_error = [ `error of string ]

type error = [
	| checksum_mismatch
	| Cmd.command_failed
	| Download.error
	| unknown_error
]

val add : key -> opam_digest list -> t -> (nix_digest, error) result Lwt.t

val add_custom : t -> keys:key list -> (unit -> (nix_digest, error) result Lwt.t) -> (nix_digest, error) result Lwt.t

val save : t -> unit Lwt.t

val try_load : key -> t

val string_of_error : error -> string
