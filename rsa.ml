open Types
open Big_int

let a_ascii = Char.code 'a'

let easy_totient p q =
  let p' = pred_big_int p in
  let q' = pred_big_int q in
  mult_big_int p' q'

let gen_private_key _ =
  let half_key_size = big_int_of_int 32 in
  let p = Mod_arith.as_big_int (Mod_arith.gen_prime half_key_size) in
  let q = Mod_arith.as_big_int (Mod_arith.gen_prime half_key_size) in
  let n = mult_big_int p q in
  let phi = easy_totient p q in
  let d = Mod_arith.gen_unit phi in
  PrivKey (d,p,q)

let get_public_key (d,p,q) =
  let n = mult_big_int p q in
  let phi = easy_totient p q in
  let N(I(e)) = Mod_arith.inv d phi in
    (PubKey(n,e))

(*Note: ocaml documents a similar verion of this function,
  but does not implement it in the standard library,
  this was found on stack overflow*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let encode_char_as_big_int c =
  let code = (Char.code c) - a_ascii + 10 in
  big_int_of_int code

let encode_string_as_big_int s =
  let shift = big_int_of_int 100 in
  let char_lst = explode s in
  let rec encode_lst_as_big_int l accum =
    match l with
    | [] -> accum
    | h::t ->
      let i = encode_char_as_big_int h in
      let accum' = mult_big_int accum shift in
      encode_lst_as_big_int t (add_big_int i accum') in
  encode_lst_as_big_int char_lst zero_big_int

let decode_big_int_as_char i =
  Char.chr ((int_of_big_int i) + a_ascii - 10)

let decode_big_int_as_string i =
  let rec decode_big_int_as_lst i accum =
    if (eq_big_int i zero_big_int) then accum
    else let i' = div_big_int i (big_int_of_int 100) in
      let r = mod_big_int i (big_int_of_int 100) in
      decode_big_int_as_lst i' (Char.escaped (decode_big_int_as_char r)::accum)
  in
  String.concat "" (decode_big_int_as_lst i [])


let encrypt (n,e) s =
  let m = encode_string_as_big_int s in
  Mod_arith.power m e n

let decrypt (d, p, q) c =
  let n = mult_big_int p q in
  let m = Mod_arith.as_big_int (Mod_arith.power c d n) in
  S(decode_big_int_as_string m)


let crack (n, e) c =
  let phi = Mod_arith.as_big_int (Mod_arith.totient n) in
  let d = Mod_arith.as_big_int (Mod_arith.inv e phi) in
  let m = Mod_arith.as_big_int (Mod_arith.power c d n) in
  S(decode_big_int_as_string m)
