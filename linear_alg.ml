
open Types

let row m n = M m

let col m n = M m

let dot_product m1 m2 = M m1

let cross_product m1 m2 = M m1

let scale m n = M m

let inverse m = M m

let transpose m = M m

let add m1 m2 = M m1

let subtract m1 m2 = M m1

let row_echelon m = M m

let red_row_echelon m = M (m)

let solve m1 m2 = M (m1)

let determinant m = N (I (Big_int.big_int_of_int 0))

let lin_ind m = N (I (Big_int.big_int_of_int 0))

let lin_dep m = N (I (Big_int.big_int_of_int 0))

let null_space m = M(m)

let col_space m = M(m)

