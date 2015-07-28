let rec filter_map fn lst =
	lst |> List.fold_left (fun acc item ->
		match fn item with
			| None -> acc
			| Some result -> result :: acc
	) [] |> List.rev

