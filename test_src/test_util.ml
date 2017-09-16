open OUnit2
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
]
