let some x = Some x
let map fn = function None -> None | Some x -> Some (fn x)
let filter fn = function None -> None | Some x -> (if fn x then Some x else None)
let may fn = function None -> () | Some x -> fn x
let bind fn = function None -> None | Some x -> fn x
let default d v = match v with Some v -> v | None -> d
let default_fn d v = match v with Some v -> v | None -> d ()
let or_else alt v = match v with Some _ -> v | None -> alt
let or_else_fn alt v = match v with Some _ -> v | None -> alt ()
let or_failwith msg v = match v with Some v -> v | None -> failwith msg
let exists fn = function None -> false | Some v -> fn v
let to_list = function None -> [] | Some v -> [v]
let is_some = function None -> false | Some _ -> true
let is_none = function None -> true | Some _ -> false
let to_string fn = function None -> "None" | Some x -> "Some(" ^ (fn x) ^ ")"
let sequence_result = function
	| None -> Ok None
	| Some (Error e) -> Error e
	| Some (Ok x) -> Ok (Some x)
