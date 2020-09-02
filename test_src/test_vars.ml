open OUnit2
open Opam2nix
module Name = OpamPackage.Name

let print_var = function
	| Some v -> OpamVariable.string_of_variable_contents v
	| None -> "None"

let test_path env prefix name ?scope expected =
	name >:: (fun _ ->
		let scope = scope |> Option.map Name.of_string in
		assert_equal ~printer:print_var (Some (S expected)) (Vars.path_var ~env ~prefix ~scope name)
	)

let suite = "Util" >:::
[
	("path vars" >:::
		let env: Vars.env = Vars.({
			packages = Name.Map.empty;
			prefix = None;
			ocaml_version = OpamPackage.Version.of_string "1.2.3";
			self = Name.of_string "mypkg";
			vars = OpamVariable.Full.Map.empty;
		}) in
		let scope = "pkg" in
		let prefix = "/prefix" in
		let test = test_path env (OpamFilename.Dir.of_string prefix) in
		let base = "/prefix/lib/ocaml/1.2.3" in
		let sitelib = base ^ "/site-lib" in
		[
			test "lib" sitelib;
			test "lib" ~scope (sitelib ^ "/pkg");
			test "stublibs" (sitelib ^ "/stublibs");
			test "stublibs" ~scope (sitelib ^ "/stublibs/pkg");
			test "toplevel" (sitelib ^ "/toplevel");
			test "toplevel" ~scope (sitelib ^ "/toplevel/pkg");
		] @ (
			(* toplevel dirs *)
			["bin"; "sbin"; "man"; "libexec"; "etc"; "doc"; "share"]
			|> List.map (fun name -> [
				test name (prefix ^ "/" ^ name);
				test name ~scope (prefix ^ "/" ^ name ^ "/" ^ scope);
			])
			|> List.concat
		) @ [
			test "lib_root" sitelib;
			test "lib_root" ~scope sitelib;
			test "share_root" (prefix ^ "/share");
			test "share_root" ~scope (prefix ^ "/share");
		]
	);
]
