module Types = struct
	type command_failed = Command_failed of (int option * string)
	type fd_spec = Inherit | Pipe | Fd of Unix.file_descr | DevNull
	type process = {
		stdin:  (Unix.file_descr, string) result;
		stdout: (Unix.file_descr, string) result;
		stderr: (Unix.file_descr, string) result;
	}
end
open Types

let string_of_command_failed (Command_failed (_, desc)) = desc ^ " failed"

module Internal = struct
	let chunk_stream fd =
		let len = 1024 in
		Stream.from (fun _ ->
			let buf = Bytes.create len in
			try
				match Unix.read fd buf 0 len with
					| 0 -> None
					| bytes_read -> Some (Bytes.sub buf 0 bytes_read)
			with Unix.Unix_error (Unix.EINTR, _, _) -> None (* TODO correct? *)
		)
	
	type bg_collector = {
		_th: Thread.t;
		_result: string ref;
	}

	let rec waitpid_with_retry flags pid =
		let open Unix in
		try waitpid flags pid
		with Unix_error (EINTR, _, _) -> waitpid_with_retry flags pid

	let pid_result ~desc pid =
		match waitpid_with_retry [ WUNTRACED ] pid with
			| (_pid, WEXITED 0) -> Ok ()
			| (_pid, WEXITED n) -> Error (Command_failed (Some n, desc))
			| (_pid, _) -> Error (Command_failed (None, desc))

	let print_desc ~print cmd: string =
		let desc = String.concat " " (Array.to_list cmd) in
		if print || Util.verbose () then prerr_endline (" + " ^ desc);
		desc

	let assert_success result = match result with
		| Ok x -> x
		| Error e -> failwith (string_of_command_failed e)
end

let collect_exn out = function
	| Ok () -> out
	| Error e -> failwith (string_of_command_failed e)

let assert_fd : (Unix.file_descr, string) result -> Unix.file_descr = function
	| Ok fd -> fd
	| Error msg -> failwith msg

let file_contents fd =
	let buf = ref Bytes.empty in
	Internal.chunk_stream fd |> Stream.iter (fun chunk -> buf := Bytes.cat !buf chunk);
	Bytes.to_string !buf |> String.trim

let file_contents_in_bg fd =
	let result = ref "" in
	let thread = Thread.create (fun () ->
		result := file_contents fd
	) () in
	Internal.({ _th = thread; _result = result })

let join_bg bg =
	let open Internal in
	Thread.join bg._th;
	!(bg._result)

let run_cmd ?(print=true) cmd =
	let desc = Internal.print_desc ~print cmd in
	let pid = Unix.create_process (Array.get cmd 0) cmd Unix.stdin Unix.stdout Unix.stderr in
	Internal.pid_result ~desc pid

let run_cmd_output ?(print=true) ~stderr cmd =
	let desc = Internal.print_desc ~print cmd in
	let (r,w) = Unix.pipe ~cloexec:true () in
	let with_stderr fn =
		if stderr then fn Unix.stderr else (
			let f = Unix.openfile "/dev/null" [ O_WRONLY] 0600 in
			let result = fn f in
			Unix.close f;
			result
		)
	in

	let pid = with_stderr (fun stderr ->
		Unix.create_process (Array.get cmd 0) cmd Unix.stdin w stderr
	) in
	let contents = file_contents r in
	match Internal.pid_result ~desc pid with
		| Ok () -> Ok contents
		| Error e -> Error e

let run_cmd_output_exn ?(print=true) cmd =
	run_cmd_output ~print ~stderr:true cmd |> Internal.assert_success

let run_cmd_exn ?(print=true) cmd =
	run_cmd ~print cmd |> Internal.assert_success

let run_cmd_output_opt ?(print=true) ?(stderr=false) cmd =
	match run_cmd_output ~print ~stderr cmd with
		| Ok output -> Some output
		| Error _ -> None

let run_cmd_bool ?(print=true) cmd =
	match run_cmd ~print cmd with
		| Ok () -> true
		| Error _ -> false

let run (type block_ret) (type ret)
	?(print=true)
	?(stdin=Inherit) ?(stdout=Inherit) ?(stderr=Inherit)
	~(collect: block_ret -> (unit, command_failed) result -> ret) (cmd: string array) (block: process -> block_ret): ret =
	let desc = Internal.print_desc ~print cmd in
	let no_fd name : (Unix.file_descr, string) result = Error
		(Printf.sprintf "%s of process %s is not a pipe" name desc) in

	let close_after_fork = ref [] in
	let close_finally = ref [] in
	let prefix ref fd = ref := fd :: !ref in

	let mkfd name (inherited:Unix.file_descr) (spec:fd_spec) = match spec with
		| Inherit -> (inherited, no_fd name)
		| DevNull when Util.verbose () -> (inherited, no_fd name)
		| Pipe ->
			let (r,w) = Unix.pipe ~cloexec:true () in
			if inherited == Unix.stdin
				then (
					prefix close_after_fork r;
					(r, Ok w)
				)
				else (
					prefix close_after_fork w;
					(w, Ok r)
				)
		| Fd fd ->
				(* assume passed FDs assume ownership, and should not outlive the process *)
				prefix close_finally fd;
				(fd, no_fd name)
		| DevNull ->
			let fd = Unix.openfile "/dev/null" [ O_RDWR] 0600 in
			prefix close_after_fork fd;
			(fd, no_fd name)
	in
	let (stdin_fd, proc_stdin) = mkfd "stdin" Unix.stdin stdin in
	let (stdout_fd, proc_stdout) = mkfd "stdout" Unix.stdout stdout in
	let (stderr_fd, proc_stderr) = mkfd "stderr" Unix.stderr stderr in
	let process = {
		stdin = proc_stdin;
		stdout = proc_stdout;
		stderr = proc_stderr;
	} in
	let cleanup ref =
		let fds = !ref in
		ref := [];
		fds |> List.iter Unix.close
	in
	try
		let pid = Unix.create_process (Array.get cmd 0) cmd stdin_fd stdout_fd stderr_fd in
		cleanup close_after_fork;
		let block_result = block process in
		let pid_result = Internal.pid_result ~desc pid in
		let result = collect block_result pid_result in
		cleanup close_finally;
		result
	with e -> (
		cleanup close_after_fork;
		cleanup close_finally;
		raise e
	)
