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
  ("simple_integer_div", "8 2 /", "4");
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
  (*test the addition two numbers mod 0*)
  ("add_mod_zero","4 5 0 +~","cannot take the remainder mod a non-positive number");
  (**test the addition two numbers mod 0 a negative number*)
  ("add_mod_neg","4 0 -63 +~","cannot take the remainder mod a non-positive number");

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
  (*tests the subtraction of two numbers mod 0*)
  ("sub_mod_zero","4 5 0 -~","cannot take the remainder mod a non-positive number");
  (*tests the subtraction of two numbers mod a non-zero number*)
  ("sub_mod_zero","4 5 -1 -~","cannot take the remainder mod a non-positive number");
  (*tests simple multiplication, which us not divisable by the modulo*)
  ("simple_mod_mult", "6 7 5 *~", "2");
  (*tests the multiplication of two numbers which is divisable by the modulo*)
  ("div_mod_mult", "9 4 6 *~", "0");
  (*tests multiplication by 0*)
  ("zero_mod_mult", "0 8 7 *~", "0");
  (*tests multiplication mod 0*)
  ("mult_mod_zero", "10 12 0 *~", "cannot take the remainder mod a non-positive number");
  (*tests multiplication mod a negative number*)
  ("mult_mod_zero", "10 0 -3 *~", "cannot take the remainder mod a non-positive number");
  (*tests division of two numbers for which the denom divides the numerator*)
  ("rel_prime&divis_mod_div", "49 7 5 /~", "2");
  (*tests the division of numbers which the denom does not divide the numerator
   * but the divisor is rel prime to the modulo*)
  ("rel_prime_mod_div", "9 5 7 /~", "6");
  (*tests the divions of numbers in which the denom is not rel prime to the modulous*)
  ("non_rel_prime_div", "10 4 2 /~", "second arguement is not relatively prime to divisor");
  (*tests the division of numbers mod 0*)
  ("div_mod_zero", "10 4 0 /~", "cannot take the remainder mod a non-positive number");
  (*test the divions of numbers mod a negative number*)
  ("div_mod_neg", "9 2 -1 /~", "cannot take the remainder mod a non-positive number");
  (*tests simple modular powers *)
  ("simpl_mod_pow", "3 4 11 ^~", "4");
  (*tests powers with a large power*)
  ("large_mod_pow", "2 243567633493504 5 ^~", "1");
  (*tests 0 to a power*)
  ("zero_mod_pow","0 829375 12 ^~", "0");
  (*tests 0 to a pow mod 0*)
  ("zero_pow_mod_zero","0 829375 0 ^~", "cannot take the remainder mod a non-positive number");
  (*tests a number to the 0 mod a number*)
  ("to_the_zero","12 0 7 ^~","1");
  (*tests powers mod 0*)
  ("pow_mod_zero", "3285 293 0 ^~", "cannot take the remainder mod a non-positive number");
  (*tests powers mod negative number*)
  ("pow_mod_neg", "0 23 -1 ^~", "cannot take the remainder mod a non-positive number");
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
  (*test the gcd with one number negative*)
  ("simple_gcd_neg", "-68 51 gcd", "17");
  (*test the gcd with the second number negative*)
  ("other_gcd_neg", "68 -51 gcd", "17");
  (*test the gcd with one number 0 number negative*)
  ("gcd_zero", "68 0 gcd", "68");
  (*test the gcd with one number diving the other*)
  ("gcd_div","999999999 9 ","9");
  (*tests a small lcm*)
  ("small_lcm", "56 62 lcm", "1736");
  (*tests the lcm of two large numbers*)
  ("large_lcm", "1624956750 14873852 lcm", "326613056836500");
  (*tests the lcm of two large negative integers*)
  ("large_lcm_neg", "-1624956750 -14873852 lcm", "326613056836500");
  (*tests the lcm with one number 0*)
  ("lcm_0", "0 12 lcm", "0");
  (*tests factoring a small number*)
  ("simple_factor", "876 factor", "(2,2) (3,1) (73,1) ");
  (*tests factoring of a large number*)
  ("largish_factor", "387153510 factor", "(2,1) (3,1) (5,1) (43,1) (300119,1) ");
  (*tests factor for a large composite number with small divisors*)
  ("large_factor_small_div","3541171240000000000 factor","(2,12) (5,10) (97,4) ");
  (*tests factor for a prime*)
  ("prime_factor","617 factor", "(617,1) ");
  (*tests factor for 0*)
  ("zero_factor","0 factor", "");
  (*tests factor for 1*)
  ("one_factor","1 factor", "");
  (*tests that a small composite is not prime*)
  ("small_composite_is_prime", "48 is_prime", "0");
  (*tests that a number is composite*)
  ("composite_is_prime", "387153510 is_prime", "0");
  (*test an is prime prob for a small number that should clearly be not prime*)
  ("small_composite_is_prime_likely", "56 is_prime_prob", "0");
  (*tests that a larger clearly composite number is not prime*)
  ("larger_is_prime_prob", "234976 is_prime_prob", "0");
  (*tests that a very large compositve nubmer is not prime*)
  ("very_large_non_prime", "856361215938558998591710718116059059061475058229 is_prime_prob", "0");
  (*tests that a very large prime is probably prime*)
  ("very_large_prime", "1582375376486522645799264544143004449330845988412246964181752084476603692779420515602503876711832499362684765390499372107885862811732185802600456088127 is_prime_prob","1");
  (*tests invalid bits for gen primes*)
  ("0_bit_prime","0 gen_prime","no primes this small");
  (*tests invalid bits for gen primes*)
  ("1_bit_prime","1 gen_prime","no primes this small");
  (*tests generating a small prime*)
  ("small_prime","3 gen_prime is_prime","1");
  (*test generating a somewhat large prime*)
  ("large_prime","200 gen_prime is_prime_prob", "1");
  (*tests finding the totient of a prime*)
  ("prime_totient", "35738783 totient", "35738782");
  (*finds a totient of a composite*)
  ("composite_totient", "532501478 totient", "266250738");
  (*tests totient of zero*)
  ("zero_totient", "0 totient", "totient undefined for 0");
  (*test totient of 1*)
  ("one_totient", "1 totient", "1")
  (*glass box*)

]

let comb_arith_tests = [
  (*black box*)
  (*tests that zero factorial is 1*)
  ("zero_fact", "0 !", "1");
  (*tests that a 1 factorial is 1*)
  ("one_fact", "1 !", "1");
  (*tests a small factorial*)
  ("small_fact", "6 !", "720");
  (*tests a larger factorial*)
  ("large_fact", "30 !", "265252859812191058636308480000000");
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
  ("simple_add_matrix", "[[2., -3.], [-4., 5.]] [[7., 34.], [56., -19.]] +",
   "[\n[ 9. 31. ]\n[ 52. -14. ]\n]");
  (*simple subtraction of two matracies*)
  ("simple_sub_matrix", "[[5., -3.], [-10., 5.], [5.6, 7.1]] [[7., 12.], [23., -19.], [13., 5.]] -",
   "[\n[ -2. -15. ]\n[ -33. 24. ]\n[ -7.4 2.1 ]\n]");
  (*simple row reduction*)
  ("simple_esch",
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
   "[\n[ 2. -4. 9. ]\n[ 0. 0. 32. ]\n[ 0. 0. 0. ]\n]");

  (*simple row reduction to reduced form*)
  ("simple_row_red",
   "[[2., -3.], [-4., 5.]] reduce",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n]");
  (*simple row reduction with more row than col to reduced form*)
  ("non_square_rr",
   "[[2., -3.], [-4., 5.], [8., 7.]] reduce",
   "[\n[ 1. 0. ]\n[ 0. 1. ]\n[ 0. 0. ]\n]");
  (*simple row red with more col than row to reduced form*)
  ("non_square_rr2",
   "[[2., -3., -1., 2.], [-4., 7., 5., 16.], [9., -70.5, 8., 7.]] reduce",
   "[\n[ 1. 0. 0. 6.19346049046 ]\n[ 0. 1. 0. 1.39509536785 ]\n[ 0. 0. 1. 6.20163487738 ]\n]");
  (*lin dep ex. to reduced form*)
  ("lin_dep_red",
   "[[2., -4., 9.], [-4., 8., 14.], [8., -16., -3.]] reduce",
   "[\n[ 1. -2. 0. ]\n[ 0. 0. 1. ]\n[ 0. 0. 0. ]\n]");
  (*tests solving a siple system of eqn*)
  ("simple_solve",
   "[[2., -3.], [-4., 5.]] [[1.], [1.]] matrix_solve",
   "[\n[ -4. ]\n[ -3. ]\n]");

]


let rsa_arith_tests = [
  (*tests generating public key from a private key computed beforehand*)
  ("gen_public_key","828508379315564229059901503743195890152288255550180775166444773564370495849 46823044180172892277581086552993376467 49226809994174581742439410872943569091 public_key", "n: 2304949099206212918857206153784356881859586917658227064795189032785987981497 e: 1613767402931803709848268727346680063096854248011089402527081469813588614409");
  (*test cracking a message encrypted with a small public key*)
  ("crack_small_key", "436127747 600282101 183129281 crack", "hi!");
  (*tests decrypting message in small public key coresponding to the
    given private key*)
  ("decrypt_small_key", "436127747 332532521 24631 24371 decrypt", "hi!");
  (*Descripts "Hello World!" when using a larger key*)
  ("decrypt_large_key","36811825527290006912528373474500742165121558710088755054352605829758001844521 49865981988111168073411299457568574845326435683397292992624028577888245241871 217732754162686778772189526558422014371 271814926074039197007457045329062122427 decrypt","Hello World!");
  (*Decripts A string ao ascii characters that are not all characters*)
  ("decrypt_strange","4681768394463021453743047398362594156618121502944021046860601989734498485 311296661144438380085479553025783322441168863090489376579001194531304074481 71131826786915666159699380046190925721 43255453359216288668744181576802303777 decrypt","{}!  #$)*")
] 


let tests = [
  simple_lang_tests;
  simpl_arith_tests;
  mod_arith_tests;
  comb_arith_tests;
  linear_arith_tests;
  rsa_arith_tests;
]

let make_tests =
  List.rev_map
    (fun (name, test, value) ->
      name >:: (fun _ -> assert_equal value (fst (Eval.evaluate_line (PMap.empty) test)) )
    )

let _ = run_test_tt_main ("suite" >::: (make_tests (List.flatten tests)))
