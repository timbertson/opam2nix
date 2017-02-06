let rec filter_map fn lst =
	lst |> List.fold_left (fun acc item ->
		match fn item with
			| None -> acc
			| Some result -> result :: acc
	) [] |> List.rev


let ends_with suffix s =
	let suffix_len = String.length suffix in
	let len = String.length s in
	len >= suffix_len && String.sub s (len - suffix_len) (suffix_len) = suffix

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
	) |> List.sort String.compare

let rec rm_r root =
	if Sys.file_exists root then (
		Sys.readdir root |> Array.to_list |> List.iter (fun name ->
			let path = (Filename.concat root name) in
			if Sys.is_directory path
				then rm_r path
				else Unix.unlink path
		);
		Unix.rmdir root
	)

let nonempty value arg =
	if value = ""
	then failwith (arg ^ " required")
	else value

let nonempty_list value arg =
	if value = []
	then failwith (arg ^ " required")
	else value

module Option = struct
	let map fn = function None -> None | Some x -> Some (fn x)
	let may fn = function None -> () | Some x -> fn x
	let bind fn = function None -> None | Some x -> fn x
	let default d v = match v with Some v -> v | None -> d
end

let rec drop n lst =
	if n <= 0
		then lst
		else match lst with
			| [] -> []
			| head :: tail -> drop (n-1) tail

let rec take n lst =
	if n <= 0
		then []
		else match lst with
			| [] -> []
			| head :: tail -> head :: (take (n-1) tail)
