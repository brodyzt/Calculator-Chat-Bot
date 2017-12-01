open Types
open Big_int
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

  let rec print_equations eqn =
    match eqn with
    | [] -> ()
    | ((x,a),(y,b),r)::t ->
      let print = print_int (int_of_big_int x ) in
      let print = print_int (int_of_big_int a ) in
      let print = print_int (int_of_big_int y ) in
      let print = print_int (int_of_big_int b ) in
      let print = print_int (int_of_big_int r ) in
      let print = print_string "\n\n" in print_equations t
    | _ -> print_string "invalid format"

let rec construct_min_bezout_sol eqn coefs =
  let print = print_equations [eqn] in
  match coefs with
  | [] -> let print = print_equations [eqn] in eqn
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


(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
let crt lst1 lst2 = P(N(I(Big_int.big_int_of_int 0)), N(I(Big_int.big_int_of_int 0)))

(*[is_square a n] is 1 if x^2 = a (mod n) for some x,
  0 if x^2 != a (mod n) for any x*)
let is_square a n = N(I(Big_int.big_int_of_int 0))
