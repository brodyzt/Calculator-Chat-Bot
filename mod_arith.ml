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

(*TODO: Make tail recursive*)
let rec add_factor lst fact =
  match lst with
  | [] -> [(fact,big_int_of_int 1)]
  | (value,freq)::t ->
    if ((compare_big_int value fact) = 0)
    then (value,(add_big_int freq (big_int_of_int 1)))::t
    else (value,freq)::(add_factor t fact)

(*NOTE: Time complexity has to be improved*)
let rec factor_helper n d accum =
  if ((compare_big_int d n) > 0) then accum
  else let r = mod_big_int n d in
    if ((compare_big_int r zero_big_int)=0)
    then factor_helper (div_big_int n d) d (add_factor accum d)
    else factor_helper n (add_big_int d (big_int_of_int 1)) accum

let factor n = Fact(factor_helper n (big_int_of_int 2) [])

let is_prime n = N(I n)

let is_prime_likely n = N(I n)

let totient n = N(I n)
