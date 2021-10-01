type error = [ `download_failed of string ]

val string_of_error : error -> string

module Ctx : sig
	type t

	val init : unit -> t

	val destroy : t -> unit
end

val fetch : Ctx.t -> dest:out_channel -> string -> (unit, [> error]) result Lwt.t
