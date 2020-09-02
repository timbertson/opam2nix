open OUnit2
let _ =
	run_test_tt_main (OUnit2.test_list [
		Test_util.suite;
		Test_vars.suite;
	])
