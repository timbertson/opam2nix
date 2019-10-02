type ('a, 'b) t = ('a, 'b) Pervasives.result

let map fn = function
	| Ok x -> Ok (fn x)
	| Error e -> Error e

let bind fn = function
	| Ok x -> fn x
	| Error e -> Error e

let iter fn = function
	| Ok x -> fn x
	| Error _ -> ()

let get_exn to_s = function
	| Ok x -> x | Error e -> failwith (to_s e)
