open OUnit2
open Repo

let print_version_list l =
	String.concat ", " (List.map Repo.string_of_version l)

let v s = Version s

let suite = "Repo" >:::
[
	"version_filter" >::: [
		"major versions" >:: (fun _ ->
			let result = version_filter [4;1] [
				v"0.1.2";
				v"0.1.0";
				v"1.0.0";
				v"1.0.1";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				v"1.0.1";
				v"0.1.2";
			] result
		);

		"drop major versions" >:: (fun _ ->
			let result = version_filter [1;2] [
				v"0.1.0";
				v"0.2.0";
				v"1.1.0";
				v"1.0.0";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				v"1.1.0";
				v"1.0.0";
			] result
		);

		"drop minor versions" >:: (fun _ ->
			let result = version_filter [1;2] [
				v"0.1.0";
				v"1.0.0";
				v"1.0.1";
				v"1.1.0";
				v"1.2.0";
				v"1.2.1";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				v"1.2.1";
				v"1.1.0";
			] result
		);

		"take from versions with inconsistent numbering" >:: (fun _ ->
			let result = version_filter [2;2;2] [
				v"0.1";
				v"0.1.2";
				v"1.0";
				v"1.0.1";
				v"1.0.2";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				v"1.0.2";
				v"1.0.1";
				v"0.1.2";
				v"0.1";
			] result
		);

		"includes +variations" >:: (fun _ ->
			let result = version_filter [1] [
				v"0.1.0+special";
				v"0.1.0";
			] |> decreasing_version_order in
			assert_equal ~printer:print_version_list [
				v"0.1.0+special";
				v"0.1.0";
			] result
		);
	]
]

