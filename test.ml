open OUnit

let simpl_arith_tests = [


]

let mod_arith_tests = [


]

let comb_arith_tests = [


]

let linear_arith_tests = [


]

let systems_arith_tests = [


]

let rsa_arith_tests = [


]


let tests = [
  simpl_arith_tests;
  mod_arith_tests;
  comb_arith_tests;
  linear_arith_tests;
  systems_arith_tests;
  rsa_arith_tests;
]

let make_tests =
  List.rev_map
    (fun (name, test, value) ->
      name >:: (fun _ -> assert_equal value (Eval.evaluate_line test))
    )

let _ = run_test_tt_main ("suite" >::: (make_tests (List.flatten tests)))