type string_component = [
	| `Lit of string
	| `Expr of t
]

and arg = [
	| `Id of string
	| `Default of string * t
]

and t = [
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

and attrset = attr list

and attr = [
  | `Expr of string * t
  | `Inherit of t option * string list
]

module AttrSet = struct
	type t = attrset

	let build pairs = List.fold_right (fun (k,v) acc ->
			(`Expr (k, v)) :: acc) pairs []

	let keys t =
		List.fold_right (fun attr acc ->
			match attr with
			| `Expr (k, _) -> k :: acc
			| `Inherit (_, keys) -> keys @ acc
		) [] t

	let add name expr t = `Expr (name, expr) :: t

	let empty = []

	let iter f t = List.iter f t
end

let str s = `String [`Lit s]

let attrset pairs = `Attrs (AttrSet.build pairs)

let apply_replacements (replacements: (Str.regexp * string) list) (s: string) : string =
	List.fold_left (fun s (re, repl) ->
		Str.global_replace re repl s
	) s replacements

let escape_string (s:string) : string =
	apply_replacements [
		(Str.regexp "\\", "\\\\");
		(Str.regexp "${", "\\${");
		(Str.regexp "\"", "\\\"");
		(Str.regexp "\n", "\\n");
		(Str.regexp "\t", "\\t");
	] s

let escape_multiline_string (s:string) : string =
	apply_replacements [
		(Str.regexp "''", "'''");
		(Str.regexp "${", "''${");
		(Str.regexp "\t", "'\\t");
	] s

let keysafe s =
	Str.string_match (Str.regexp "^[-a-zA-Z_][-a-zA-Z_0-9]*$") s 0

let escape_key s = if keysafe s then s else "\"" ^ (escape_string s) ^ "\""

let write dest (t:t) =
	let open Format in
	let formatter = formatter_of_out_channel dest in
	let indent_width = 2 in
	let put = pp_print_string formatter in
	let nl = pp_force_newline formatter in
	let space = pp_print_space formatter in
	let rec _write (t:t) =
		let dbl = "\"" in
		let two_singles = "''" in
		let string_component (escape: string -> string) c = match c with
			| `Lit s -> put (escape s)
			| `Expr s -> put "${"; _write s; put "}"
		in
		let parens_if_needed part =
			match part with
				(* for neatness, we don't bother enclosing simple expressions in parens *)
				| `Id _ | `Int _ | `Lit _ | `String _ | `MultilineString _ | `List _ | `Attrs _ | `Rec_attrs _ -> _write part
				| _ -> put "("; _write part; put ")"
		in
		let property name = put ("." ^ (escape_key name)) in

		let write_inherit expr = function
			| [] -> ()
			| keys ->
				nl ();
				put "inherit";
				space ();
				begin match expr with
					| None -> ();
					| Some expr ->
						put "(";
						_write expr;
						put ")";
						space ();
				end;
				pp_print_list pp_print_string ~pp_sep:pp_print_space formatter keys;
				put ";"
		in

		let write_attrs ~prefix a =
			pp_print_cut formatter ();
			pp_open_box formatter indent_width;
			put prefix;
			put "{";
			a |> AttrSet.iter (function
				| `Inherit (expr, keys) -> write_inherit expr keys
				| `Expr (key, v) ->
				(* XXX what about quoted keys? *)
				nl ();
				put (if keysafe key then key else "\"" ^ (escape_string key) ^ "\"");
				put " = ";
				_write v;
				put ";";
			);
			pp_close_box formatter ();
			nl ();
			put "}";
		in

		match t with
			| `String parts ->
				put dbl;
				parts |> List.iter (string_component escape_string);
				put dbl
			| `MultilineString parts ->
				put two_singles;
				parts |> List.iter (string_component escape_multiline_string);
				put two_singles
			| `List parts ->
				put "[";
				pp_open_box formatter indent_width;
				space ();
				parts |> List.iteri (fun i part ->
					if i > 0 then space ();
					parens_if_needed part
				);
				space ();
				pp_close_box formatter ();
				put "]"
			| `Id id -> put id
			| `Int i -> put (string_of_int i)
			| `Lit str -> put str
			| `Null -> put "null"
			| `BinaryOp (a, op, b) ->
					let bracket expr = put "("; _write expr; put ")" in
					bracket a; put " "; put op; put " "; bracket b
			| `Property (src, name) -> parens_if_needed src; property name
			| `PropertyPath (src, path) -> parens_if_needed src; path |> List.iter property
			| `Property_or (src, name, alt) ->
					_write (`Property (src, name));
					put " or ";
					_write alt
			| `Function (args, body) ->
					_write args;
					put ":";
					nl ();
					_write body;
					nl ();
			| `Call args ->
					args |> List.iteri (fun i arg ->
						if i > 0 then space ();
						parens_if_needed arg;
					)
			| `Let_bindings (vars, expr) ->
				put "let";
				(* pp_print_cut formatter (); *)
				pp_open_box formatter 1;
				vars |> AttrSet.iter (function
					| `Inherit (expr, keys) -> write_inherit expr keys
					| `Expr (key, v) ->
						(* XXX what about quoted keys? *)
						nl ();
						put key;
						put " = ";
						_write v;
						put ";"
				);
				pp_close_box formatter ();
				nl ();
				put "in";
				nl ();
				_write expr
			| `NamedArguments parts ->
					put "{ ";
					pp_open_box formatter indent_width;
					parts |> List.iteri (fun i part ->
						if i <> 0 then (put ","; space ());
						match part with
							| `Id arg -> put arg
							| `Default (arg, exp) ->
									put arg;
									put " ? ";
									_write exp
					);
					pp_close_box formatter ();
					space ();
					put "}";
			| `Template parts ->
				parts |> List.iter (function
					| `Lit s -> put s
					| `Expr e -> _write e
				)
			| `Rec_attrs a -> write_attrs ~prefix:"rec " a
			| `Attrs a -> write_attrs ~prefix:"" a
			| `With (scope, expr) ->
					put "with ";
					_write scope;
					put "; ";
					_write expr;
	in
	pp_open_box formatter 0;
	_write t;
	pp_close_box formatter ();
	pp_print_newline formatter ();
