open OUnit
open Types


let simple_lang_tests = [
  (*black box lang tests*)
  ("simple_int", "4", "4");
  ("negative_int", "-78", "-78");
  ("overflowing_int", "1000000000000000000000000000000000000", "1000000000000000000000000000000000000" );
  ("simple_float", "5.0", "5.");
  ("negative_float", "-3479.2498", "-3479.2498");
  ("simple_matrix", "[[2., 3.], [4., 5.]]", "[\n[ 2. 3. ]\n[ 4. 5. ]\n]");
  ("simple_matrix_with_neg", "[[2., -3.], [-4., 5.]]", "[\n[ 2. -3. ]\n[ -4. 5. ]\n]");
  ("simple_string", {|"message"|}, "message");
  ("simple_if_false", "0 2 3 ?", "3");
  ("simple_if_true", "1 2 3 ?", "2");

  (*glass box lang tests*)

]

let simpl_arith_tests = [
  (*blackbox test*)
  ("simple_integer_add", "2 3 +", "5");
  ("simple_float_add", "2. 3. +", "5.");
  ("simple_integer_subt", "2 3 -", "-1");
  ("simple_float_subt", "10. 3. -", "7.");
  ("simple_integer_mult", "9 5 *", "45");
  ("simple_float_mult", "2.5 7. *", "17.5");
  ("simple_integer_div", "8 2 /", "3");
  ("simple_float_div", "27. 5. /", "5.4");
  ("simple_integer_mod", "100 3 %", "1");
  ("simple_integer_pow", "2 10 ^", "1024");
  ("simple_float_pow", "10. 3. ^", "1000.");
  ("simple_integer_=", "18 17 =", "0");
  ("simple_float_=", "15.2 15.3 =", "0");

  (*glass box*)

]

let mod_arith_tests = [
  (*black box*)
  ("simple_mod_add", "7 6 2 +~", "1");
  ("larger_mod_add", "21 34 7 +~", "6");
  ("non_div_mod_add", "5 6 11 +~", "0");
  ("simple_mod_sub", "9 4 2 -~", "1");
  ("0_mod_sub", "7 7 20 +~", "0");
  ("no_div_mod_sub", "37 20 17 +~", "0");
  ("larger_mod_sub", "483275 1 3 +~", "1");
  ("simple_mod_mult", "6 7 5 *~", "2");
  ("div_mod_mult", "9 4 6 *~", "0");
  ("zero_mod_mult", "0 8 7 *~", "0");
  ("rel_prime&divis_mod_div", "49 7 5 /~", "2");
  ("rel_prime_mod_div", "9 5 7 /~", "6");
  ("simpl_mod_pow", "3 4 11 ^~", "4");
  ("large_mod_pow", "2 243567633493504 5 ^~", "0");
  ("simple_mod_eq", "16 2 15 =~", "0");
  ("large_mod_eq", "726476239857380 52 358893 =~", "0");



  (*glass box*)
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
  simple_lang_tests;
  (*simpl_arith_tests;*)
  (*mod_arith_tests;*)
  comb_arith_tests;
  linear_arith_tests;
  systems_arith_tests;
  rsa_arith_tests;
]

let make_tests =
  List.rev_map
    (fun (name, test, value) ->
      name >:: (fun _ -> assert_equal value (fst (Eval.evaluate_line (PMap.empty) test)) )
    )

let _ = run_test_tt_main ("suite" >::: (make_tests (List.flatten tests)))