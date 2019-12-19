include Seq
let rec of_list elems () = match elems with
	| [] -> Nil
	| head :: tail -> Cons (head, of_list tail)
