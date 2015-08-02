let rec filter_map fn lst =
	lst |> List.fold_left (fun acc item ->
		match fn item with
			| None -> acc
			| Some result -> result :: acc
	) [] |> List.rev


let without_leading prefix s =
	let prefix_len = String.length prefix in
	let len = String.length s in
	if len >= prefix_len && String.sub s 0 prefix_len = prefix
	then Some (String.sub s prefix_len (len - prefix_len))
	else None

let without_trailing suffix s =
	let suffix_len = String.length suffix in
	let len = String.length s in
	if len >= suffix_len && String.sub s (len - suffix_len) (suffix_len) = suffix
	then Some (String.sub s 0 (len - suffix_len))
	else None

let list_dirs root =
	Sys.readdir root |> Array.to_list |> List.filter (fun name ->
		Sys.is_directory (Filename.concat root name)
	)


let nonempty value arg =
	if value = ""
	then failwith (arg ^ " required")
	else value

module Option = struct
	let map fn = function None -> None | Some x -> Some (fn x)
end
