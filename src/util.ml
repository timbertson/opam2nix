let identity x = x

let filter_map fn lst =
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

let id x = x

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
end

let rec drop n lst =
	if n <= 0
		then lst
		else match lst with
			| [] -> []
			| _ :: tail -> drop (n-1) tail

let rec take n lst =
	if n <= 0
		then []
		else match lst with
			| [] -> []
			| head :: tail -> head :: (take (n-1) tail)

let head_opt = function
	| x::_ -> Some x
	| [] -> None

let tail = function
	| [] -> []
	| _::x -> x

let group_by : 'item 'key. ('item -> 'key) -> 'item list -> ('key * 'item list) list
= fun fn items ->
	let finish key items_rev = (key, List.rev items_rev) in
	let rec accum groups_rev current_key current_group_rev = function
		| [] -> (* end of inputs *)
			(if (current_group_rev = [])
				then []
				else [finish current_key current_group_rev]
			) @ groups_rev
		| head::tail ->
			let key = fn head in
			if (key = current_key)
				then accum groups_rev current_key (head :: current_group_rev) tail
				else (
					let groups_rev = (finish current_key current_group_rev) :: groups_rev in
					accum groups_rev key [head] tail
				)
	in
	match items with
		| [] -> []
		| head::tail ->
			let key = fn head in
			accum [] key [head] tail |> List.rev

let fst = function (a,_) -> a
let snd = function (_,b) -> b

let explode s =
	let rec exp i l =
		if i < 0 then l else exp (i - 1) (s.[i] :: l) in
		exp (String.length s - 1) []

let string_of_char = String.make 1

(* This is a bit ad-hoc.
 * We represent non-safe characters as +xNN, where NN is the hex representation.
 * Only supports ASCII. Literal +x is encoded (as +x2b+x78) *)
let encode_nix_safe_path str =
	let encode ch =
		let a,b = (Hex.of_char ch) in
		"+x" ^ (string_of_char a) ^ (string_of_char b)
	in
	let open Str in
	full_split (regexp "[^.+_a-zA-Z0-9-]\\|\\+x") str |> List.map (function
		| Delim x when x = "+x" -> (encode '+' ^ encode 'x')
		| Delim x -> String.concat "" (List.map encode (explode x))
		| Text x -> x
	) |> String.concat ""

let decode_nix_safe_path str =
	let open Str in
	let hex = "[0-9a-fA-F]" in
	full_split (regexp ("\\+x" ^ hex ^ hex)) str |> List.map (function
		| Delim x -> Hex.to_char x.[2] x.[3] |> String.make 1
		| Text x -> x
	) |> String.concat ""

module List = struct
	include List
	let to_string fn lst = "[" ^ (String.concat ", " (map fn lst)) ^ "]"
end

let _verbose = ref false

let verbose () = !_verbose

let set_verbose v =
	if v then Printf.eprintf "Verbose output enabled\n";
	_verbose := v

let debug fmt = (if verbose () then Printf.eprintf else Printf.ifprintf stderr) fmt

let () = (
	let envvar = try Unix.getenv "OPAM2NIX_VERBOSE" with Not_found -> "" in
	set_verbose (envvar = "1" || envvar = "true")
)

module StringMap = struct
	include Map.Make(String)
	let singleton key value = add key value empty
	let find_opt key map = try Some (find key map) with Not_found -> None
	let from_list items = List.fold_right (fun (k,v) map -> add k v map) items empty
end

type command_failed = Command_failed of (int option * string)
let string_of_command_failed (Command_failed (_, desc)) = desc ^ " failed"

type fd_spec = Inherit | Pipe | Fd of Unix.file_descr | DevNull
type process = {
	stdin:  (Unix.file_descr, string) result;
	stdout: (Unix.file_descr, string) result;
	stderr: (Unix.file_descr, string) result;
}

(* TODO extract into its own module, clean up *)
module Cmd = struct
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
	
	let file_contents fd =
		let buf = ref Bytes.empty in
		chunk_stream fd |> Stream.iter (fun chunk -> buf := Bytes.cat !buf chunk);
		Bytes.to_string !buf |> String.trim

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
		if print || verbose () then prerr_endline (" + " ^ desc);
		desc

	let assert_success result = match result with
		| Ok x -> x
		| Error e -> failwith (string_of_command_failed e)

	let collect_exn out = function
		| Ok () -> out
		| Error e -> failwith (string_of_command_failed e)

	let assert_fd : (Unix.file_descr, string) result -> Unix.file_descr = function
		| Ok fd -> fd
		| Error msg -> failwith msg

	let read_stdout proc =
		file_contents (proc.stdout |> assert_fd)

	let file_contents_in_bg fd =
		let result = ref "" in
		let thread = Thread.create (fun () ->
			result := file_contents fd
		) () in
		{ _th = thread; _result = result }

	let join_bg bg =
		Thread.join bg._th;
		!(bg._result)

end

let run_cmd ?(print=true) cmd =
	let desc = Cmd.print_desc ~print cmd in
	let pid = Unix.create_process (Array.get cmd 0) cmd Unix.stdin Unix.stdout Unix.stderr in
	Cmd.pid_result ~desc pid

let run_cmd_output ?(print=true) ~stderr cmd =
	let desc = Cmd.print_desc ~print cmd in
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
	let contents = Cmd.file_contents r in
	match Cmd.pid_result ~desc pid with
		| Ok () -> Ok contents
		| Error e -> Error e

let run_cmd_output_exn ?(print=true) cmd =
	run_cmd_output ~print ~stderr:true cmd |> Cmd.assert_success

let run_cmd_exn ?(print=true) cmd =
	run_cmd ~print cmd |> Cmd.assert_success

let run_cmd_output_opt ?(print=true) ?(stderr=false) cmd =
	match run_cmd_output ~print ~stderr cmd with
		| Ok output -> Some output
		| Error _ -> None

let run_cmd_bool ?(print=true) cmd =
	match run_cmd ~print cmd with
		| Ok () -> true
		| Error _ -> false

let run_cmd_full (type block_ret) (type ret)
	?(print=true)
	?(stdin=Inherit) ?(stdout=Inherit) ?(stderr=Inherit)
	~(collect: block_ret -> (unit, command_failed) result -> ret) (cmd: string array) (block: process -> block_ret): ret =
	let desc = Cmd.print_desc ~print cmd in
	let no_fd name : (Unix.file_descr, string) result = Error
		(Printf.sprintf "%s of process %s is not a pipe" name desc) in

	(* closing FDs:
	 * After we fork, we close all file descriptors that we gave to the child
	 * (except for inherited ones).
	 * After a process has finished, we close the ???
	 *)
	let close_after_fork = ref [] in
	let close_finally = ref [] in
	let prefix ref fd = ref := fd :: !ref in

	let mkfd name (inherited:Unix.file_descr) (spec:fd_spec) = match spec with
		| Inherit -> (inherited, no_fd name)
		| DevNull when verbose () -> (inherited, no_fd name)
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
		let pid_result = Cmd.pid_result ~desc pid in
		let result = collect block_result pid_result in
		cleanup close_finally;
		result
	with e -> (
		cleanup close_after_fork;
		cleanup close_finally;
		raise e
	)
