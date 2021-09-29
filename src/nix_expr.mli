
type string_component

type t = [
	| `String of string_component list
	| `MultilineString of string_component list
	| `List of t list
	| `Property of t * string
	| `PropertyPath of t * string list
	| `Property_or of t * string * t
	| `Attrs of attrset
	| `Rec_attrs of attrset
	| `NamedArguments of arg list
	| `Function of t * t
	| `Id of string
	| `Int of int
	| `Let_bindings of attrset * t
	| `Call of t list
	| `Template of string_component list
	| `Lit of string
	| `BinaryOp of t * string * t
	| `Null
	| `With of (t * t)
]

and arg = [
	| `Id of string
	| `Default of string * t
]

and attrset = attr list

and attr = [
  | `Expr of string * t
  | `Inherit of t option * string list
]

module AttrSet : sig
	type t = attrset

	type expr

	val empty : t

	val build : (string * expr) list -> t

	val add : string -> expr -> t -> t
end with type expr := t

val attrset : (string * t) list -> t

val str : string -> t

val write : out_channel -> t -> unit
