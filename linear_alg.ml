
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
    if l1 <> l2 then M(Array.make_matrix 0 0 (F(0.)) )
    else
      let rec mult n =
        if n = l1 then () else
          ((match m1.(n).(0), m2.(n).(0) with
          | F(f1), F(f2) -> r.(n).(0) <- F(f1 *. f2)
          | _ -> failwith "unimplemented"); mult (n+1))
      in mult 0; M(r)



let cross_product m1 m2 =
  let l1 = Array.length m1 in
  let l2 = Array.length m2 in
  let r = Array.make_matrix l1 1 (F(0.) ) in
    if l1 <> l2 || l1 <> 3 then M(Array.make_matrix 0 0 (F(0.)) )
    else
      match m1.(0).(0), m1.(0).(1), m1.(0).(1), m2.(0).(0), m2.(0).(1), m2.(0).(2) with
      | F(a1), F(a2), F(a3), F(b1), F(b2), F(b3) -> begin
        r.(0).(0) <-  F(a2 *. b3 +. a3 *. b2);
        r.(0).(1) <-  F(a3 *. b1 +. a1 *. b3);
        r.(0).(2) <-  F(a1 *. b2 +. a2 *. b1);
        M(r)
      end

let scale m n =
  match n with
  | I k -> M(Array.map (fun r -> Array.map (fun (F v) -> F((float_of_big_int k) *.v )) r) m)
  | F f -> M(Array.map (fun r -> Array.map (fun (F v) -> F(f *.v )) r) m)

let inverse m = M m

let transpose m =
  let res = Array.make_matrix (Array.length m.(0)) (Array.length m) (F(0.) ) in
    Array.iteri (fun i r -> Array.iteri (fun j v -> res.(j).(i) <- v ) r ) m; M(res)

let add m1 m2 =
  let n1 = Array.length m1 in
  let n2 = Array.length m2 in
  let o1 = Array.length (m1.(0)) in
  let o2 = Array.length (m2.(0)) in
  if n1 = n2 && o1 = o2 then
    M(Array.map2 (fun r1 r2 -> Array.map2 (fun (F a) (F b) -> F(a +. b) ) r1 r2) m1 m2)
  else
    M(Array.make_matrix 0 0 (F(0.)) )

let subtract m1 m2 =
  let n1 = Array.length m1 in
  let n2 = Array.length m2 in
  let o1 = Array.length (m1.(0)) in
  let o2 = Array.length (m2.(0)) in
  if n1 = n2 && o1 = o2 then
    M(Array.map2 (fun r1 r2 -> Array.map2 (fun (F a) (F b) -> F(a -. b) ) r1 r2) m1 m2)
  else
    M(Array.make_matrix 0 0 (F(0.)) )

let clear_col j i1 i2 =
  let F(x), F(y) = i2.(j), i1.(j) in
  let p = x /. y in
    (print_string ( (string_of_float x) ^ (string_of_float y) ^ (string_of_float p) );
    Array.mapi (fun i (F v) -> let F(y) = i1.(i) in F(v -. y *. p) ) i2 )

let rec find_non_zero m i j =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      if m.(i).(j) <> F(0.) then i else find_non_zero m (i+1) j
    else -1

let swap m i1 i2 =
  let temp = m.(i1) in
    m.(i1) <- m.(i2);
    m.(i2) <- temp

let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> string_of_float f

let string_of_matrix m =
  "[\n"^(Array.fold_right ( fun  e ac ->
    "[ "^(Array.fold_right (fun  el acc -> (string_of_number el)^" "^acc) e ("]\n"^ac))  )
    m
    "]"
     )

let rec red_row_down m i j =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      if m.(i).(j) = F( 0.) then
        let non_zero = find_non_zero m (i+1) j in
          if non_zero = -1 then red_row_down m i (j+1)
          else
            let (rr,s) = (swap m i non_zero; red_row_down m i j) in
              (rr,s+1)
      else
        (Array.iteri (fun ind row -> if ind > i then m.(ind) <- (clear_col j ( m.(i) ) row ) else () ) m;
        print_string (string_of_matrix m);
        red_row_down m (i+1) (j+1))
    else
     (M(m),0)

let row_echelon m =
  fst (red_row_down (Array.map (fun row -> Array.copy row) m) 0 0)

let find_pivot m i =
  let rec piv j =
    if j >= Array.length m.(i) then -1
    else if m.(i).(j) <> F(0.) then j else piv (j+1)
  in piv i


let rec red_row_up m i=
  if i >= 0 then
    let j = find_pivot m i in
      if j = -1 then red_row_up m (i-1) else
        let F(piv_val) = m.(i).(j) in
          ( Array.iteri (fun j' (F v) -> m.(i).(j') <- F(v /. piv_val) ) m.(i);
            Array.iteri (fun ind row -> if ind < i then m.(ind) <- (clear_col j ( m.(i) ) row ) else () ) m;
            red_row_up m (i-1))
  else
    M(m)

let red_row_echelon m =
  let M(ech) = row_echelon m in
    red_row_up ech (Array.length ech -1)

let read_off_sol a m =
  let len = Array.length a.(0) in
    Array.iteri (fun i row -> m.(i).(0) <- row.(len-1)) a; m

let solve m1 m2 =
  let rows1 = Array.length m1 in
  let rows2 = Array.length m2 in
    if rows1 = 0 || Array.length m1.(0) = 0 || rows1 = rows2 then
      let aug = Array.make_matrix rows1 ((Array.length m1.(0)) +1) (F 0.) in
        Array.iteri (fun i row -> Array.iteri (fun j (F v) -> aug.(i).(j) <- m1.(i).(j)) row) m1;
        Array.iteri (fun i row -> aug.(i).(Array.length m1.(0)) <- row.(0)) m2;
          let M(sol) = red_row_echelon aug in
            M(read_off_sol (sol) m2)
    else
     E "matrix size issue"

let rec prod_diag m i j acc =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      match m.(i).(j) with
      | F(f) -> prod_diag m (i+1) (j+1) (acc *. f)
      | _ -> failwith "unimplemented"
    else acc


let determinant m =
  let M(rr),swap_count = red_row_down m 0 0 in
    N(F(prod_diag m 0 0 (if swap_count mod 2 = 1 then -1. else 1.)))


let lin_ind m =
  if determinant m = N(F(0.)) then N (I (Big_int.big_int_of_int 0)) else N (I (Big_int.big_int_of_int 1))

let lin_dep m =
  if determinant m = N(F(0.)) then N (I (Big_int.big_int_of_int 1)) else N (I (Big_int.big_int_of_int 0))

let null_space m = M(m)

let col_space m = M(m)

let eq m1 m2 =
  let r1 = Array.length m1 in
  let c1 = Array.length (m1.(0)) in
  let r2 = Array.length m2 in
  let c2 = Array.length (m2.(0)) in
    if r1 = r2 && c1 = c2 then
      let e = ref 1 in
        Array.iter2 (fun r1 r2 -> Array.iter2 (fun v1 v2 -> if v1 = v2 then () else e := !e * 0) r1 r2) m1 m2;
        N(I(big_int_of_int (!e)))
    else
      N(I(big_int_of_int (0)))
