open Types
open Big_int

let easy_totient p q =
  let p' = pred_big_int p in
  let q' = pred_big_int q in
  mult_big_int p' q'

let gen_private_key _ =
  let half_key_size = big_int_of_int 8 in
  let p = Mod_arith.as_big_int (Mod_arith.gen_prime half_key_size) in
  let q = Mod_arith.as_big_int (Mod_arith.gen_prime half_key_size) in
  let n = mult_big_int p q in
  let phi = easy_totient p q in
  let d = Mod_arith.gen_unit phi in
  PrivKey (d,p,q)

let get_public_key (d,p,q) =
  let n = mult_big_int p q in
  let phi = easy_totient p q in
  let e = Mod_arith.inv d phi in
  P(N(I(n)),e)

let encrypt_int (n,e) i = Mod_arith.power e i n
let encrypt (n,e) s =
  let i = big_int_of_string s in encrypt_int (n,e) i

let decrypt (p, q, d) c = N(I (Big_int.big_int_of_int 0))

let crack (n, e) c = N(I (Big_int.big_int_of_int 0))
