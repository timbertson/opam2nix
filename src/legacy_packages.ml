module Map = StringMap

let mapping () =
	(* TODO: this is dependant on the state of the nixpkgs repository,
	 * it probably shouldn't be so hardcoded...
	 *)
	let prepend_ocaml = [ "lwt"; ] in
	let pairs = List.concat([
		prepend_ocaml |> List.map (fun name -> ("ocaml_"^name, name));
		[
			"m4", "ocamlm4";
		]
	]) in
	List.reduce Map.empty (fun map (k,v) -> Map.add k v map) pairs
