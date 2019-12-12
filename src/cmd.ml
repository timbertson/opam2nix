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

let lwt_file_contents = let open Lwt_io in fun fd ->
	read ?count:None fd

let lwt_stdout_contents proc =
	lwt_file_contents proc#stdout

let cmd_of_list x = ("", x |> Array.of_list)

let with_command_result cmd block = fun proc ->
	Lwt.both
		(block proc)
		(proc#status |> Lwt.map (Internal.result_of_status ~cmd))

type 'ret exec_ret = ('ret * (unit, command_failed) result) Lwt.t

let run_lwt (type block_ret) (type ret) (type proc)
	(runner: (proc -> block_ret Lwt.t) -> string list -> block_ret exec_ret)
	?(print=true)
	~(join: block_ret -> (unit, command_failed) result -> ret)
	~(block: proc -> block_ret Lwt.t)
	(cmd: string list)
	: ret Lwt.t =
	let _ = Internal.print_desc ~print cmd in
	runner block cmd |> Lwt.map (fun (a,b) -> join a b)

let exec_r ?stdin ?stderr block cmd =
	Lwt_process.with_process_in ?stdin ?stderr (cmd_of_list cmd) (with_command_result cmd block)

let exec_w ?stdout ?stderr block cmd =
	Lwt_process.with_process_out ?stdout ?stderr (cmd_of_list cmd) (with_command_result cmd block)

let exec_rw ?stderr block cmd =
	Lwt_process.with_process ?stderr (cmd_of_list cmd) (with_command_result cmd block)

let exec_none ?stdin ?stdout ?stderr block cmd =
	Lwt_process.with_process_none ?stdin ?stdout ?stderr (cmd_of_list cmd) (with_command_result cmd block)

let lwt_run_exn spawn = run_lwt spawn ~join:join_exn

let lwt_run_unit spawn = run_lwt spawn ~block:ignore_lwt
let lwt_run_unit_exn spawn = lwt_run_unit spawn ~join:join_exn
let lwt_run_unit_result = lwt_run_unit ~join:join_result

let lwt_run_output ?print ~join cmd = run_lwt exec_r ?print ~join ~block:lwt_stdout_contents cmd
let lwt_run_output_exn = lwt_run_output ~join:join_exn
let lwt_run_output_result = lwt_run_output ~join:join_result
let lwt_run_output_opt = lwt_run_output ~join:join_opt

