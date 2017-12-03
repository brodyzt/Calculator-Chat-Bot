open Types
open Mod_arith
open Simpl_arith
open Systems_eqs
open Big_int

let gen_private_key _ =
  let half_key_size = big_int_of_int 1014 in
  let p = as_big_int (gen_prime half_key_size) in
  let q = as_big_int (gen_prime half_key_size) in
  let n = mult_big_int p q in
  let d = gen_unit n in
  PrivKey (d,p,q)

let get_public_key (p,q,d) =
  let n = mult_big_int p q in
  let e = inv d n in
  P(e,N(I(n)))


let encrypt (n, e) s = N(I (Big_int.big_int_of_int 0))

let decrypt (p, q, d) c = N(I (Big_int.big_int_of_int 0))

let crack (n, e) c = N(I (Big_int.big_int_of_int 0))
