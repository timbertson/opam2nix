open Printf
open Util

let commands : (string * (int -> string array -> unit)) list = [
	"generate", Generate.main;
	"invoke", Invoke.main;
	"select", Select.main;
]

let () =
	if Array.length Sys.argv = 0 then (
		eprintf "Usage: opam2nix <command> [args]\n\nAvailable commands: %s"
		(commands |> List.map (fun (name, _) -> name) |> String.concat ", ");
		exit 1
	) else (
		let commandName = Sys.argv.(0) in
		let command =
			try Some (commands |> List.find (fun (name, action) -> name = commandName))
			with Not_found -> None in
		match command with
			| Some (_name, action) -> action 1 Sys.argv
			| None -> eprintf "Unknown command: %s\n" commandName; exit 1
	)
