open Lwt.Infix

module Types = struct
	type command_failed = Command_failed of (int option * string list)
end
open Types

module Internal_ = struct
	let desc cmd = String.concat " " cmd

	let result_of_status ~cmd = let open Unix in function
		| WEXITED 0 -> Ok ()
		| WEXITED n -> Error (Command_failed (Some n, cmd))
		| _ -> Error (Command_failed (None, cmd))

	let print_desc ~print cmd =
		if print || Util.verbose () then prerr_endline (" + " ^ (desc cmd))
end

let string_of_command_failed (Command_failed (_, cmd)) = (Internal_.desc cmd) ^ " failed"

module Internal = struct
	include Internal_
	let assert_success result = match result with
		| Ok x -> x
		| Error e -> failwith (string_of_command_failed e)
end
open Internal

let join_result out = function
	| Ok () -> Ok out
	| Error e -> Error e

let join_exn out = function
	| Ok () -> out
	| Error e -> failwith (string_of_command_failed e)

let join_opt result = function
	| Ok () -> Some result
	| Error _ -> None

let join_success_bool () = function
	| Ok () -> true
	| Error _ -> false

let ignore_lwt _ = Lwt.return_unit

let file_contents = let open Lwt_io in fun fd ->
	read ?count:None fd

let stdout_contents proc =
	file_contents proc#stdout

let cmd_of_list x = ("", x |> Array.of_list)

let run
	(spawn: Lwt_process.command -> 'proc)
	?(print=true)
	~(join: 'block_ret -> (unit, command_failed) result -> 'ret)
	~(block: 'proc -> 'block_ret Lwt.t)
	(cmd: string list)
	: 'ret Lwt.t =
	Internal.print_desc ~print cmd;
	let proc = spawn (cmd_of_list cmd) in
	Lwt.try_bind
		(fun () -> block proc)
		(fun result ->
			proc#close |> Lwt.map (result_of_status ~cmd) |> Lwt.map (join result)
		)
		(fun err -> proc#close >>= fun _ -> Lwt.fail err)

let exec_r ?stdin ?stderr = Lwt_process.open_process_in ?stdin ?stderr
let exec_w ?stdout ?stderr = Lwt_process.open_process_out ?stdout ?stderr
let exec_rw ?stderr = Lwt_process.open_process ?stderr
let exec_none ?stdin ?stdout ?stderr = Lwt_process.open_process_none ?stdin ?stdout ?stderr

(* handy shortcuts *)

let run_exn spawn = run spawn ~join:join_exn

let run_unit spawn = run spawn ~block:ignore_lwt
let run_unit_exn spawn = run_unit spawn ~join:join_exn
let run_unit_result = run_unit ~join:join_result

let run_output ?print ~join cmd = run exec_r ?print ~join ~block:stdout_contents cmd
let run_output_exn = run_output ~join:join_exn
let run_output_result = run_output ~join:join_result
let run_output_opt = run_output ~join:join_opt

