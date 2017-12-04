open Types
open Big_int
include Random

let is_init = ref false

let as_big_int i =
  match i with
  | N(I(x)) -> x
  | _ -> failwith "precondition violated"
let as_big_int_pair p =
  match p with
  | P(N(I(x)),N(I(y))) -> (x,y)
  | _ -> failwith "precondition violated"
let truthy b =
  match b with
  | N(I(x)) -> if eq_big_int x zero_big_int then false else true
  | _ -> failwith "precondition violated"
let add a b n =
  if ((compare_big_int n zero_big_int) <= 0) then E("cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (add_big_int a b) n))

let subtract a b n =
  if ((compare_big_int n zero_big_int) <= 0) then E("cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (sub_big_int a b) n))

let multiply a b n =
  if ((compare_big_int n zero_big_int) <= 0) then (E "cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (mult_big_int a b) n))



(*only works for positive nums*)

let rec as_bin_list a accum =
  if eq_big_int a zero_big_int then accum
  else let (q,r) = quomod_big_int a (big_int_of_int 2) in
    if eq_big_int r zero_big_int then as_bin_list q (zero_big_int::accum)
    else as_bin_list q (unit_big_int::accum)

let rec repeated_square a n exp accum =
  match accum with
  | [] -> repeated_square a n exp ((mod_big_int a n)::accum)
  | h::t ->
    let x_sqr= as_big_int (multiply h h n) in
    if eq_big_int exp zero_big_int then accum
    else repeated_square a n (pred_big_int exp) (x_sqr::h::t)

let rec condense n pows bin accum=
  match pows,bin with
  | [],[] -> accum
  | h1::t1,h2::t2 ->
    if (eq_big_int h2 zero_big_int)
    then condense n t1 t2 accum
    else condense n t1 t2 (as_big_int (multiply h1 accum n))
  | _,_ -> failwith "pows and bin must be the same lenth"

let power a b n =
  let b = abs_big_int b in
  if (eq_big_int b zero_big_int) then N(I(unit_big_int))
  else if ((compare_big_int n zero_big_int) <= 0) then E("cannot take the remainder mod a non-positive number")
  else let a_red = mod_big_int a n in
  if eq_big_int a_red zero_big_int then N(I(zero_big_int))
  else let bin = as_bin_list b [] in
  let expn = big_int_of_int ((List.length bin) - 1) in
  let squares = repeated_square a n expn [] in
    N(I(condense n squares bin unit_big_int))


let rec pow_factor p n accum=
  let r = (mod_big_int n p) in
  if ((compare_big_int r zero_big_int)=0)
  then pow_factor p (div_big_int n p) (add_big_int accum (big_int_of_int 1))
  else accum

let rec factor_helper n d accum =
  if ((compare_big_int n (big_int_of_int 1))=0) then accum
  else if ((compare_big_int (square_big_int d) n)> 0)
  then (n,big_int_of_int 1)::accum
  else let pow = pow_factor d n (zero_big_int) in
    if((compare_big_int pow zero_big_int)=0)
    then factor_helper n (add_big_int d (big_int_of_int 1)) accum
    else let n' = div_big_int n (power_big_int_positive_big_int d pow) in
      factor_helper n' d ((d,pow)::accum)

let factor n =
  let one = big_int_of_int 1 in
  if (((compare_big_int n zero_big_int)=0) || ((compare_big_int n one)=0))
  then Fact([])
  else Fact(List.rev (factor_helper n (big_int_of_int 2) []))

let is_prime n =
  if ((compare_big_int n unit_big_int)<=0) then N(I(zero_big_int))
  else let res = factor n in
  match res with
  | Fact factors -> begin
      match factors with
      | [] -> N(I(zero_big_int))
      | (fact,freq)::t ->
        if (((compare_big_int freq (big_int_of_int 1))=0) && (t = []))
        then N(I(big_int_of_int 1))
        else N(I(zero_big_int))
    end
  | _ -> failwith "factored incorrectly"

let eq a b n =
  match subtract a b n with
  | E e -> E e
  | N v1 -> begin
      match v1 with
      | I v2 -> if ((compare_big_int v2 zero_big_int) = 0) then N(I(big_int_of_int 1))
        else N(I(zero_big_int))
      | _ -> failwith "Unimplemented"
    end
  | _ -> failwith "unimplemented"

let rec gcd a b =
  if ((compare_big_int a zero_big_int) < 0) then gcd (minus_big_int a) b
  else if ((compare_big_int b zero_big_int) < 0) then gcd a (minus_big_int b)
  else if ((compare_big_int a b) < 0) then gcd b a
  else if ((compare_big_int b zero_big_int) = 0) then N(I(a))
  else gcd b (mod_big_int a b)

let lcm a b =
  match gcd a b with
  | N (I v) -> N(I (div_big_int (mult_big_int a b) v))
  | _ -> failwith "unreachable case"

let ensure_rand_init _ =
  if not(!is_init) then begin Random.self_init (); is_init := true end
  else ()

let rec gen_rand_bits_helper num_bits accum =
  ensure_rand_init ();
  if eq_big_int num_bits zero_big_int then accum
  else let is_one = Random.bool () in
    if is_one
    then gen_rand_bits_helper (pred_big_int num_bits) (unit_big_int::accum)
    else gen_rand_bits_helper (pred_big_int num_bits) (zero_big_int::accum)

let rec gen_rand_bits num_bits = gen_rand_bits_helper num_bits []

let rec big_int_of_bit_list_helper bits pow accum =
  match bits with
  | [] -> accum
  | h::t ->
    if (eq_big_int h unit_big_int)
    then let pow_two = power_big_int_positive_big_int (big_int_of_int 2) pow in
      big_int_of_bit_list_helper t (succ_big_int pow) (add_big_int accum pow_two)
    else big_int_of_bit_list_helper t (succ_big_int pow) accum

let big_int_of_bit_list bits =
  big_int_of_bit_list_helper bits zero_big_int zero_big_int

let gen_rand_big_int n = big_int_of_bit_list (gen_rand_bits n)

let rec get_num_bits_helper n accum =
  if eq_big_int zero_big_int n then accum
  else get_num_bits_helper (div_big_int n (big_int_of_int 2)) (succ_big_int accum)

let get_num_bits n = get_num_bits_helper n zero_big_int

let fermats_little a n =
  let result = power a (pred_big_int n) n in
  if (eq_big_int (as_big_int result) unit_big_int) then true
  else false

let rec is_prime_k_tests_helper n k num_bits =
  if (eq_big_int k zero_big_int) then true
  else let a = gen_rand_big_int num_bits in
    if (fermats_little a n)
    then is_prime_k_tests_helper n (pred_big_int k) num_bits
    else false

let rec is_prime_k_tests n k =
  (*hardcoded for now*)
  if lt_big_int n (big_int_of_int 1000000) then
    truthy (is_prime (n))
  else let bits = get_num_bits n in
    is_prime_k_tests_helper n k bits


let is_prime_likely n =
  (*hardcoded for now*)
  let prob_prime = is_prime_k_tests n (big_int_of_int 100) in
  if prob_prime then N(I(unit_big_int))
  else N(I(zero_big_int))

let rec gen_prime_helper n =
  let p = gen_rand_big_int n in
  if truthy (is_prime_likely p) then p
  else gen_prime_helper n

let gen_prime l =
  if ((compare_big_int l unit_big_int)<=0) then E("no primes this small")
  else N(I(gen_prime_helper l))

let rec gen_unit_helper n bits =
  let u = gen_rand_big_int bits in
  if eq_big_int (as_big_int (gcd u n)) unit_big_int then u
  else gen_unit_helper n bits

let gen_unit n =
  let num_bits = get_num_bits n in
  gen_unit_helper n (pred_big_int num_bits)

let rec totient_helper factors accum =
  match factors with
  | [] -> accum
  | (factor,pow)::t ->
    let term1 = (power_big_int_positive_big_int factor pow) in
    let term2 = div_big_int term1 factor in
    let accum' = mult_big_int (sub_big_int term1 term2) accum in
    totient_helper t accum'

let totient n =
  if ((compare_big_int n zero_big_int)<=0) then E("totient undefined for 0")
  else let res = factor n in
  match res with
  | Fact factors -> N(I(totient_helper factors (big_int_of_int 1)))
  | _ -> failwith "n must be factorable"


(*[gen_bezout_ceofs a b coefs] is a list of (v,(x',a'),(b') such that
  v = x'*a' + b', for each of the values a',b' that occur when recursively
  applying the gcd function to a and b*)
let rec gen_bezout_coefs a b coefs =
  if (eq_big_int b zero_big_int) then (a,(zero_big_int,zero_big_int),a)::coefs
  else let (q,r) = quomod_big_int a b in
    let coefs' = (a,(q,b),r)::coefs in
    gen_bezout_coefs b r coefs'

let merge_coefs ((x,b),(y,r),r') ((a),(q,b),r) =
  let x' = minus_big_int y in
  let y' = minus_big_int (add_big_int x (mult_big_int y q)) in
  ((x',a),(y',b),r')

let rec construct_min_bezout_sol eqn coefs =
  match coefs with
  | [] -> eqn
  | h::t -> construct_min_bezout_sol (merge_coefs eqn h) t
(*[bezout a b c] is a pair (x, y) where x*a + y*b = c, or an exception value
  if no such pair exists*)
let get_x_y_gcd coefs =
  match coefs with
  | [] -> failwith "error cannot have no coefficients"
  | ((a),(q,b),r)::t ->
    let ((x,_),(neg_y,_),res) =
      construct_min_bezout_sol ((big_int_of_int 1,a),(q,b),r) t in
    (x,minus_big_int neg_y, res)

let bezout a b c =
  let coefs = gen_bezout_coefs a b [] in
  let (x,y,res) = get_x_y_gcd coefs in
  if eq_big_int zero_big_int (mod_big_int c res)
  then let m = div_big_int c res in
    P(N(I(mult_big_int m x)),N(I(mult_big_int m y)))
  else E("gcd(a,b) does not divide c, so no solution exists")

let join_congruence_pair bi mi bj mj =
  if not(eq_big_int (as_big_int (gcd mi mj)) unit_big_int)
  then E("not relatively prime")
  else let m' = mult_big_int mi mj in
    let coefs = as_big_int_pair (bezout mi mj unit_big_int) in
    let fst_term = as_big_int (multiply (mult_big_int (fst coefs) mi) bj m') in
    let snd_term = as_big_int (multiply (mult_big_int (snd coefs) mj) bi m') in
    let res = (add fst_term snd_term m') in
    P(res,N(I(m')))

let rec crt_helper lst1 lst2 accum =
  match lst1,lst2 with
  | [],[] -> accum
  | bi::t1,mi::t2 ->
    let (b,m) = as_big_int_pair accum in
    let accum' = join_congruence_pair bi mi b m in
    crt_helper t1 t2 accum'
  | _,_ -> E("lists are not the same length")
(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
let crt lst1 lst2 =
  match lst1,lst2 with
  | [],_ -> E("must supply at least one congruence equation")
  | _,[] -> E("must supply at least one congruence equation")
  | b0::t1,m0::t2 -> crt_helper t1 t2 (P(N(I(b0)),N(I(m0))))

(*[is_square a p] is 1 if x^2 = a (mod n) for some x,
  0 if x^2 != a (mod n) for any x*)
let is_square a p =
  let two = big_int_of_int 2 in
  if (le_big_int p zero_big_int)
  then E("cannot take the remainder mod a non-positive number")
  else if (le_big_int p two) then N(I(unit_big_int))
  else
    let a_red = mod_big_int a p in
    let p_minus_one = pred_big_int p in
    let p_minus_one_div_two = div_big_int p_minus_one two in
    let legendre = as_big_int (power a_red p_minus_one_div_two p) in
    if (eq_big_int legendre (p_minus_one)) then N(I(zero_big_int))
    else N(I(unit_big_int))

let inv a n =
  if ((compare_big_int n zero_big_int) <= 0) then E("cannot take the remainder mod a non-positive number")
  else let result = bezout a n (big_int_of_int 1) in
      match result with
        | E _ -> E("has no inverse mod this number")
        | P (N(I(x)),_) -> N(I(mod_big_int x n))
        | _ -> failwith "Unimplemented"

let divide a b n =
  if ((compare_big_int n zero_big_int)<=0) then E("cannot take the remainder mod a non-positive number")
  else
  let result = inv b n in
  match result with
    | E _ -> E("second arguement is not relatively prime to divisor")
    | N(I(b_inv)) -> multiply a b_inv n
    | _ -> failwith "Unimplemented"
