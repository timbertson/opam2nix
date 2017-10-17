open OUnit2
open Opam2nix
open Util

let print_string x = x

let test_encode str expected =
	"encode " ^ str >:: (fun _ ->
		assert_equal ~printer:print_string expected (encode_nix_safe_path str)
	)

let test_decode str expected =
	"decode" >:: (fun _ ->
		assert_equal ~printer:print_string expected (decode_nix_safe_path str)
	)

let suite = "Util" >:::
[
	"filename encoding" >::: [
		test_encode "hello there" "hello+x20there";
		test_encode "hello	there" "hello+x09there";
		test_encode "++x+x" "++x2b+x78+x2b+x78";
		test_encode "xxx" "xxx";
		test_encode "+++" "+++";

		test_decode "hello+x20there" "hello there";
		test_decode "hello+x09there" "hello	there";
		test_decode "++x2b+x78+x2b+x78" "++x+x";
	];

	"group_by" >:::
			let printer = fun groups -> (
				let inner = groups |> List.map (fun (b, nums) ->
					Printf.sprintf "(%b, [%s])" b (nums |> List.map string_of_int |> String.concat "; ")
				) in
				Printf.sprintf "[%s]" (String.concat "; " inner)
			) in
			let assert_equal = assert_equal ~printer in
			let group_by_zero = group_by (fun x -> x = 0) in
		[

		"single run" >:: (fun _ ->
			assert_equal [
				(false, [1;2;3])
			]
			(group_by_zero [1;2;3])
		);

		"multiple runs" >:: (fun _ ->
			assert_equal [
				(false, [1;2;3]);
				(true, [0]);
				(false, [4;5]);
				(true, [0;0]);
			]
			(group_by_zero [1;2;3;0;4;5;0;0])
		);
	];
]
