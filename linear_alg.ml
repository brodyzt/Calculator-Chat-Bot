
open Types
open Big_int

let row m n =
  let i = int_of_big_int n in
    if i < 0 || i >= Array.length m then M(Array.make_matrix 0 0 (F(0.)) )
    else
      let r = Array.make_matrix 1 (Array.length m) (F(0.) ) in
      let rec convert j =
        if j < 0 then () else r.(0).(j) <- m.(j).(i); convert (j-1)
      in convert ((Array.length m) -1); M(r)

let col m n =
  let i = int_of_big_int n in
    if i < 0 || i >= Array.length m.(0) then M(Array.make_matrix 0 0 (F(0.)) )
    else
      let r = Array.make_matrix 1 (Array.length m) (F(0.) ) in
      let rec convert j =
        if j < 0 then () else r.(0).(j) <- m.(i).(j); convert (j-1)
      in convert ((Array.length m.(i)) -1); M(r)

let dot_product m1 m2 =
  let l1 = Array.length m1 in
  let l2 = Array.length m2 in
  let r = Array.make_matrix l1 1 (F(0.) ) in
  print_string (string_of_int (Array.length r));
    if l1 <> l2 then M(Array.make_matrix 0 0 (F(0.)) )
    else
      let rec mult n =
        print_string (string_of_int n);
        if n = l1 then () else
          ((match m1.(n).(0), m2.(n).(0) with
          | F(f1), F(f2) -> r.(n).(0) <- F(f1 *. f2)
          | _ -> failwith "unimplemented"); mult (n+1))
      in mult 0; M(r)



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

let eq m1 m2 = N (I (Big_int.big_int_of_int 0))