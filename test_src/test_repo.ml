open OUnit2
open Repo

let print_version_list l =
	String.concat ", " l

let suite = "Repo" >:::
[
	"version_filter" >::: [
		"major versions" >:: (fun _ ->
			let result = version_filter 4 [
				"0.1.2";
				"0.1.0";
				"1.0.0";
				"1.0.1";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				"1.0.1";
				"0.1.2";
			] result
		);

		"drop old versions" >:: (fun _ ->
			let result = version_filter 2 [
				"0.1.0";
				"0.2.0";
				"1.1.0";
				"1.0.0";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				"1.1.0";
				"1.0.0";
			] result
		);

	]
]

