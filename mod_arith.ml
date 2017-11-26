open Types
open Big_int

let add a b n =
  if ((compare_big_int n zero_big_int) <= 0) then E("cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (add_big_int a b) n))

let subtract a b n =
  if ((compare_big_int n zero_big_int) <= 0) then E("cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (sub_big_int a b) n))

let multiply a b n =
  if ((compare_big_int n zero_big_int) <= 0) then (E "cannot take the remainder mod a non-positive number")
  else N(I(mod_big_int (mult_big_int a b) n))

let divide a b n = N(I a)


let power a b n = N(I a)

(*
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
*)


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

let gen_prime l = N(I l)

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
  let res = factor n in
  match res with
  | Fact factors -> begin
      match factors with
      | [] -> failwith "an integer cannot have no factors"
      | (fact,freq)::t ->
        if (((compare_big_int freq (big_int_of_int 1))=0) && (t = []))
        then N(I(big_int_of_int 1))
        else N(I(zero_big_int))
    end
  | _ -> failwith "factored incorrectly"

let is_prime_likely n = N(I n)

let rec totient_helper factors accum =
  match factors with
  | [] -> accum
  | (factor,pow)::t ->
    let term1 = (power_big_int_positive_big_int factor pow) in
    let term2 = div_big_int term1 factor in
    let accum' = mult_big_int (sub_big_int term1 term2) accum in
    totient_helper t accum'

let totient n =
  if ((compare_big_int n zero_big_int)=0) then N(I(zero_big_int))
  else let res = factor n in
  match res with
  | Fact factors -> N(I(totient_helper factors (big_int_of_int 1)))
  | _ -> failwith "n must be factorable"
