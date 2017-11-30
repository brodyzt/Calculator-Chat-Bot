open OUnit
open Types


let simple_lang_tests = [
  (*black box lang tests*)
  (*tests basic integer*)
  ("simple_int", "4", "4");
  (*tests a negative integer*)
  ("negative_int", "-78", "-78");
  (*tests an int that would normally overflow*)
  ("overflowing_int", "1000000000000000000000000000000000000",
    "1000000000000000000000000000000000000" );
  (*tests a simple floating point value  *)
  ("simple_float", "5.0", "5.");
  (*tests a larger float which is negative*)
  ("negative_float", "-3479.2498", "-3479.2498");
  (*tests simple 4 by 4 matrix*)
  ("simple_matrix", "[[2., 3.], [4., 5.]]", "[\n[ 2. 3. ]\n[ 4. 5. ]\n]");
  (*tests a simple 4 by 4 matrix with negative integers*)
  ("simple_matrix_with_neg", "[[2., -3.], [-4., 5.]]",
    "[\n[ 2. -3. ]\n[ -4. 5. ]\n]");
  (*tests a simple string*)
  ("simple_string", {|"message"|}, "message");
  (*tests a simple if, if the value if the first one is a false (0) value*)
  ("simple_if_false", "0 2 3 ?", "3");
  (*tests a simple if, if the value if the first one is a true (non 0) value*)
  ("simple_if_true", "1 2 3 ?", "2");

  (*glass box lang tests*)

]

let simpl_arith_tests = [
  (*blackbox test*)
  (*tests a simple addition of integers*)
  ("simple_integer_add", "2 3 +", "5");
  (*tests addition of simple floating point numbers*)
  ("simple_float_add", "2. 3. +", "5.");
  (*tests simple subtraction of integers which results in a negative integer*)
  ("simple_integer_subt", "2 3 -", "-1");
  (*tests simple subtraction of floating point numbers*)
  ("simple_float_subt", "10. 3. -", "7.");
  (*tests simple multiplication of integer values*)
  ("simple_integer_mult", "9 5 *", "45");
  (*tests multiplication of floating point numbers*)
  ("simple_float_mult", "2.5 7. *", "17.5");
  (*tests simple integer division*)
  ("simple_integer_div", "8 2 /", "3");
  (*tests simple floating point division*)
  ("simple_float_div", "27. 5. /", "5.4");
  (*tests modulus for integers*)
  ("simple_integer_mod", "100 3 %", "1");
  (*tests taking powers of two relativly small integers*)
  ("simple_integer_pow", "2 10 ^", "1024");
  (*tests powers of the small floating point numbers*)
  ("simple_float_pow", "10. 3. ^", "1000.");
  (*tests the equality of two numbers*)
  ("simple_integer_=", "18 17 =", "0");
  (*tests the equality of floating point numbers*)
  ("simple_float_=", "15.2 15.3 =", "0");

  (*glass box*)

]

let mod_arith_tests = [
  (*black box*)
  (*test addition of small numbers whose sum is not divisable by the modulo*)
  ("simple_mod_add", "7 6 2 +~", "1");
  (*tests addition with some larger numbers*)
  ("larger_mod_add", "21 34 7 +~", "6");
  (*tests the addition of two numbers which is not divisible by the modulo
   * but their sum is*)
  ("non_div_mod_add", "5 6 11 +~", "0");
  (*tests the simple modular subtraction of 2 numbers which is not divisable by
   * the modulo*)
  ("simple_mod_sub", "9 4 2 -~", "1");
  (*tests the subtraction of the same number which would be 0 regardless of mod*)
  ("0_mod_sub", "7 7 20 -~", "0");
  (*tests the subtraction of two numbers which is non zero, but is divisable
   * by the modulo*)
  ("no_div_mod_sub", "37 20 17 -~", "0");
  (*tests the subtraction of two larger numbers*)
  ("larger_mod_sub", "483275 34261 3 -~", "1");
  (*tests simple multiplication, which us not divisable by the modulo*)
  ("simple_mod_mult", "6 7 5 *~", "2");
  (*tests the multiplication of two numbers which is divisable by the mdulo*)
  ("div_mod_mult", "9 4 6 *~", "0");
  (*tests multiplication by 0*)
  ("zero_mod_mult", "0 8 7 *~", "0");
  (*testsdivision of two numbers which the denom divides the numerator*)
  ("rel_prime&divis_mod_div", "49 7 5 /~", "2");
  (*tests the division of numbers which the denom does not divide the numerator
   * but the divisor is rel prime to the modulo*)
  ("rel_prime_mod_div", "9 5 7 /~", "6");
  (*tests simple modular powers *)
  ("simpl_mod_pow", "3 4 11 ^~", "4");
  (*tests powers with a large power*)
  ("large_mod_pow", "2 243567633493504 5 ^~", "0");
  (*tests simple equality of two small numbers*)
  ("simple_mod_eq", "16 2 15 =~", "0");
  (*tests the modular equality with large numbers*)
  ("large_mod_eq", "726476239857380 52 358893 =~", "0");
  (*tests the gcd of two small numbers which have a common division above 1*)
  ("simple_gcd", "68 51 gcd", "17");
  (*tests the gcd of two large rel prime numbers*)
  ("large_1_gcd", "100000037 1000000345537 gcd", "1");
  (*tests the gcd of two large numbers which are not rel prime*)
  ("large_1_gcd", "8753735081401 9696994354185197 gcd", "411301747");
  (*tests a small lcm*)
  ("small_lcm", "56 62 lcm", "1736");
  (*tests the lcm of two large numbers*)
  ("large_lcm", "1624956750 14873852 lcm", "326613056836500");
  (*tests factoring a small number*)
  ("simple_factor", "876 factor", "(2,2) (3,1) (73,1) ");
  (*tests factoring of a large number*)
  ("largish_factor", "387153510 factor", "(2,1) (3,1) (5,1) (43,1) (300119,1) ");
  (*tests that a small composite is not prime*)
  ("small_composite_is_prime", "48 is_prime", "0");
  (*tests that a number which is composite*)
  ("composite_is_prime", "387153510 is_prime", "0");
  (*test an is prime prob for a small number that should clearly be not prime*)
  ("small_composite_is_prime_likely", "56 is_prime_prob", "0");
  (*tests that a larger clearly composite number is not prime*)
  ("larger_is_prime_prob", "234976 is_prime_prob", "0");
  (*tests finding the totient of a prime*)
  ("prime_totient", "35738783", "35738782");
  (*finds a totient of a composite*)
  ("composite_totient", "532501478", "266250738");

  (*glass box*)

]

let comb_arith_tests = [
  (*black box*)
  (*tests a small factorial*)
  ("small_fact", "6 !", "120");
  (*tests a larger factorial*)
  ("large_fact", "30", "265252859812191058636308480000000");
  (*computes a small combination*)
  ("small_choose", "13 5 choose", "1287");
  (*computes a large choose*)
  ("large_choose", "217 43 choose", "5601414076770489401221861478881318576914682800");
  (*computes a small permutation*)
  ("small_perm", "10 4 perm", "5040");
  (*computes a large perm*)
  ("large_perm", "49 25 perm", "980390734080409707851586040233984000000");
  (*need partition tests*)

]

let linear_arith_tests = [
  (*simple adding of two matricies*)

  (*simple subtraction of two matracies*)

  (*simple row reduction*)
  ("simple_row_red",
   "[[2., -3.], [-4., 5.]] echelon",
   "[\n[ 2. -3. ]\n[ 0. -1. ]\n]");
  (*simple row reduction with more row than col*)
  ("non_square_rr",
   "[[2., -3.], [-4., 5.], [8., 7.]] echelon",
   "[\n[ 2. -3. ]\n[ 0. -1. ]\n[ 0. 0. ]\n]");
  (*simple row red with more col than row*)
  ("non_square_rr2",
   "[[2., -3., -1., 2.], [-4., 7., 5., 16.], [9., -70.5, 8., 7.]] echelon",
   "[\n[ 2. -3. -1. 2. ]\n[ 0. 1. 3. 20. ]\n[ 0. 0. 183.5 1138. ]\n]");
  (*lin dep ex.*)
  ("lin_dep_red",
   "[[2., -4., 9.], [-4., 8., 14.], [8., -16., -3.]] echelon",
   "[\n[ 2. -4. 9. ]\n[ 0. 0. 32. ]\n[ 0. 0. 0. ]\n]")

  (*simple row reduction to reduced form*)
  ("simple_row_red",
   "[[2., -3.], [-4., 5.]] echelon",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n]");
  (*simple row reduction with more row than col to reduced form*)
  ("non_square_rr",
   "[[2., -3.], [-4., 5.], [8., 7.]] echelon",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n[ 0. 0. ]\n]");
  (*simple row red with more col than row to reduced form*)
  ("non_square_rr2",
   "[[2., -3., -1., 2.], [-4., 7., 5., 16.], [9., -70.5, 8., 7.]] echelon",
   "[\n[ 1. 0. 0. 6.19346049046 ]\n[ 0. 1. 0. 1.39509536785 ]\n[ 0. 0. 1. 6.20163487738 ]\n]");
  (*lin dep ex. to reduced form*)
  ("lin_dep_red",
   "[[2., -4., 9.], [-4., 8., 14.], [8., -16., -3.]] echelon",
   "[\n[ 1. 2. 0. ]\n[ 0. 0. 1. ]\n[ 0. 0. 0. ]\n]")

]

let systems_arith_tests = [


]

let rsa_arith_tests = [


]


let tests = [
  simple_lang_tests;
  (*simpl_arith_tests;*)
  (*mod_arith_tests;*)
  (*comb_arith_tests;*)
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