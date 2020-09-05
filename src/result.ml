type ('a, 'b) t = ('a, 'b) Stdlib.result

let ok x = Ok x

let map fn = function
	| Ok x -> Ok (fn x)
	| Error e -> Error e

let bind fn = function
	| Ok x -> fn x
	| Error e -> Error e

let iter fn = function
	| Ok x -> fn x
	| Error _ -> ()

let tap fn = function
	| Ok x -> fn x; Ok x
	| Error _ as e -> e

let get_exn to_s = function
	| Ok x -> x | Error e -> failwith (to_s e)

let or_raise = function
	| Ok x -> x | Error e -> raise e

let map_err f = function
	| Ok x -> Ok x | Error e -> Error (f e)