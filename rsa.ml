open Eval

let gen_private_key _ = N(I (Big_int.big_int_of_int 0))

let get_public_key (p,q,d) = P(N(I (Big_int.big_int_of_int 0)),N(I (Big_int.big_int_of_int 0)))

let encrypt (n, e) s = N(I (Big_int.big_int_of_int 0))

let decrypt (p, q, d) c = N(I (Big_int.big_int_of_int 0))

let crack (n, e) c = N(I (Big_int.big_int_of_int 0))