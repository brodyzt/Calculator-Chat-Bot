
open Types
open Big_int

(*[init_matrix x y f] creates a 2D matrix with [x] rows and [y] cols
 * using the function [f] to get the value for each cell*)
let init_matrix x y f =
  Array.init x (fun i -> Array.init y (fun j ->  f i j))

(*[iterij f] iterates over a 2D array applying the function f to each of the
 * cells*)
let iterij f =
  Array.iteri (fun i row -> Array.iteri (fun j v -> f i j v) row)

(*[non_zero v] tests that [v] is not zero*)
let non_zero v =
  match v with
  | I(i) -> not (eq_big_int i zero_big_int)
  | F(f) -> f <> 0.
  | Q(a,b) -> not (eq_big_int a zero_big_int)

let zero v =
  match v with
  | I _ -> I(zero_big_int)
  | F _ -> F(0.0)
  | Q _ -> Q(zero_big_int, unit_big_int)

let row m n =
  try
    let i = int_of_big_int n in
      if i < 0 || i >= Array.length m.(0) then M(Array.make_matrix 0 0 (F(0.)) )
      else
        M(init_matrix 1 (Array.length m) (fun _ j -> m.(i).(j)))
  with Failure _ -> (E "n is to large")

let col m n =
  try
    let i = int_of_big_int n in
      if i < 0 || i >= Array.length m then M(Array.make_matrix 0 0 (F(0.)) )
      else
        M(init_matrix (Array.length m) 1 (fun i' _ -> m.(i').(i)))
  with Failure _ -> E("n is too large")


let dot_product m1 m2 =
  let l1 = Array.length m1 in
  let l2 = Array.length m2 in
    if (Array.length m1.(0)) <> 1 ||
      (Array.length m1.(0)) <> (Array.length m2.(0)) ||
      l1 <> l2 then E("matrix size issue")
    else
      let rec mult n sum =
        if n = l1 then sum else
          let N(prod) = Simpl_arith.multiply m1.(n).(0) m2.(n).(0) in
          let N(new_sum) = (Simpl_arith.add sum prod) in
            mult (n+1) new_sum
      in N(mult 0 (zero m1.(0).(0)))

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
      | I(a1), I(a2), I(a3), I(b1), I(b2), I(b3) -> begin
        r.(0).(0) <-  I(add_big_int (mult_big_int a2 b3) (mult_big_int a3 b2));
        r.(0).(1) <-  I(add_big_int (mult_big_int a3 b1) (mult_big_int a1 b3));
        r.(0).(2) <-  I(add_big_int (mult_big_int a1 b2) (mult_big_int a2 b1));
        M(r)
      end

(*[ap_num f i v] applys the function [f] to [v] if v is a float value and applys
 * [i] to [v] if [v] is an integer value*)
let ap_num f i v =
  match v with
  | F(v') -> F(f v')
  | I(v') -> I(i v')

(*[app_num f i a b] applies the function [f] to [a] and [b] if either [a]
 * or [b] is a float otherwise the function [i] is used*)
let app_num f i a b =
  match a,b with
  | F(a'), F(b') -> F(f a' b')
  | F(a'), I(b') -> F(f a' (float_of_big_int b') )
  | I(a'), F(b') -> F(f (float_of_big_int a') b')
  | I(a'), I(b') -> I(i a' b')

let scale m n =
  match n with
  | I k -> begin
    M(Array.map
      (fun r -> Array.map
        (ap_num (fun v -> (float_of_big_int k) *.v) (fun v -> mult_big_int k v))
         r)
       m)
  end
  | F f -> begin
    M(Array.map
      (fun r -> Array.map
        (ap_num (fun v -> (f *.v )) (fun v -> mult_big_int (big_int_of_int (int_of_float f) ) v))
         r)
       m)
  end

let transpose m =
  let res = Array.make_matrix (Array.length m.(0)) (Array.length m) (F(0.) ) in
    Array.iteri (fun i r -> Array.iteri (fun j v -> res.(j).(i) <- v ) r ) m;
    M(res)

(*[simple_bin f i m1 m2] applies the function [f] to the float entries of the
 * matricies [m1] and [m2] to combine them and applys the [i] function to float
 * entries*)
let simple_bin f i m1 m2 =
  let n1 = Array.length m1 in
  let n2 = Array.length m2 in
  let o1 = Array.length (m1.(0)) in
  let o2 = Array.length (m2.(0)) in
  if n1 = n2 && o1 = o2 then
    M(Array.map2
      (fun r1 r2 -> Array.map2
        (app_num f i )
         r1 r2)
      m1 m2)
  else
    E("matrix size issue")

let add =
  simple_bin (+.) (add_big_int)

let subtract =
  simple_bin (-.) (sub_big_int)

let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> string_of_float f
  | Q (a, b) -> (string_of_big_int a )^ "/" ^ (string_of_big_int b)

let string_of_matrix m =
  "[\n"^(Array.fold_right ( fun  e ac ->
    "[ "^(Array.fold_right (fun  el acc -> (string_of_number el)^" "^acc) e ("]\n"^ac))  )
    m
    "]"
     )

(*[clear_col j i1 i2] substracts a multiple of i1 from i2 such that the
 * leading entry of i2 will become 0(the leading entry is in the [j]
 * col)*)
let clear_col j i1 i2 =
  let N(p) = (Simpl_arith.divide  i2.(j) i1.(j) ) in
    print_string (string_of_number p);
    Array.mapi
    (fun i v ->
       let N(mul) = (Simpl_arith.multiply i1.(i) p ) in
       let N(value) = (Simpl_arith.subtract v mul) in
         value
    ) i2

(*
    (fun i -> ((app_num
      (fun y v ->
        let F(a), F(b) = i1.(j), i2.(j) in
        let p = b /. a in
          v -. y *. p)
      (fun y v ->
        let I(a), I(b) = i1.(j), i2.(j) in
        let p = div_big_int b a in
          sub_big_int v (mult_big_int y p) )) i1.(i))
    ) i2
  *)

(*[fin_non_zero m i j] findes the first non_zero entry in the col [j] of
 * the matrix [m]*)
let rec find_non_zero m i j =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      if non_zero m.(i).(j) then i else find_non_zero m (i+1) j
    else -1

(*[swap m i1 i2] swaps the [i1] and [i2] rows in the matrix [m]*)
let swap m i1 i2 =
  let temp = m.(i1) in
    m.(i1) <- m.(i2);
    m.(i2) <- temp



let convert_to_rat m =
  match m.(0).(0) with
  | I _ -> (init_matrix (Array.length m) (Array.length m.(0)) (fun i j -> let I(n) = m.(i).(j) in Q(n, big_int_of_int 1) ) )
  | F _ -> m
  | Q _ -> m

(*[red_row_down m i j] reduces the [m] into row reduced form where the matrix
 * row [i] and col [j] is already of that form that result is then paired with
 * the number of row swaps made though the process*)
let rec red_row_down m i j =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && j < cols then
      match m.(i).(j) with
      | (F 0.0) -> begin
        let non_zero = find_non_zero m (i+1) j in
          if non_zero = -1 then red_row_down m i (j+1)
          else
            let (rr,s) = (swap m i non_zero; red_row_down m i j) in
              (rr,s+1)
      end
      | Q (z, _ ) when (eq_big_int z (zero_big_int))-> begin
          let non_zero = find_non_zero m (i+1) j in
            if non_zero = -1 then red_row_down m i (j+1)
            else
              let (rr,s) = (swap m i non_zero; red_row_down m i j) in
                (rr,s+1)
      end
      | _ ->
        (Array.iteri
          (fun ind row ->
            if ind > i then
              (print_string (string_of_matrix m);
              m.(ind) <- (clear_col j ( m.(i) ) row ))
            else ()
          ) m;
          red_row_down m (i+1) (j+1))
    else
     (M(m),0)

let row_echelon m =
  let m = convert_to_rat m in
   fst (red_row_down (Array.map (fun row -> Array.copy row) m) 0 0)
(*[find_pivot m i] finds the pivot (first non zero number) in the row [i]
 * of the matrix [m]*)
let find_pivot m i =
  let rec piv j =
    if j >= Array.length m.(i) then -1
    else if non_zero m.(i).(j) then j else piv (j+1)
  in piv i

(*[red_row_up m i] transforms the matrix from row echelon form to row
 * reduced form where [m] below the row [i] is alrady of that form*)
let rec red_row_up m i=
  if i >= 0 then
    let j = find_pivot m i in
      if j = -1 then red_row_up m (i-1) else
        let piv_val = m.(i).(j) in
          (print_string (string_of_int i);
           Array.iteri (fun j' v ->
            print_int j';
            let N(n) = Simpl_arith.divide v (piv_val) in
              m.(i).(j') <- n
          ) m.(i);
          Array.iteri (fun ind row -> if ind < i then m.(ind) <- (clear_col j ( m.(i) ) row ) else () ) m;
          red_row_up m (i-1))
  else
    M(m)

let red_row_echelon m =
  let M(ech) = row_echelon m in
    red_row_up ech (Array.length ech -1)



(*[read_off_inv aug m] given the augmented matrix [aug] which has twice as many
 * col as [m] transfers the inverse of m into the matrix m it self which will be
 * the right square matrix in [aug]*)
let read_off_inv aug m =
  let cols = Array.length (m.(0)) in
    iterij (fun i j v -> m.(i).(j) <- aug.(i).(j+cols)) m

let inverse m =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if rows = cols && rows <> 0 then
      let z,o =
        match m.(0).(0) with
        | I _ -> (I (big_int_of_int 0)),(I (big_int_of_int 1))
        | F _ -> (F 0.), F(1.)
      in
      let aug = (init_matrix rows (cols*2)
          (fun i j ->
            if j < cols then m.(i).(j)
            else if i = j-cols then o else z)
         ) in
        let M(solved) = red_row_echelon aug in
          (read_off_inv solved m; M(m))
    else
      E "matrix size error"

(*[prod_diag m i j acc] takes the product of the entries along the primary
 * diagonal of [m] where the entries to the left and above [i] have already been
 * taken into account
 * requires: m has been reduced to row eschelon form *)
let rec prod_diag m i acc =
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
    if i < rows && i < cols then
      match m.(i).(i), acc with
      | F(f), F(a) -> prod_diag m (i+1) (F(a *. f))
      | I(n), I(a) -> prod_diag m (i+1) (I (mult_big_int a n))
    else acc


let determinant m =
  let M(rr),swap_count = red_row_down m 0 0 in
    (*needs to work for ints too*)
    N(prod_diag m 0 (F(if swap_count mod 2 = 1 then -1. else 1.)))


let lin_ind m =
  if determinant m = N(F(0.)) then N (I (Big_int.big_int_of_int 0)) else N (I (Big_int.big_int_of_int 1))

let lin_dep m =
  if determinant m = N(F(0.)) then N (I (Big_int.big_int_of_int 1)) else N (I (Big_int.big_int_of_int 0))


(*[piv_col m f init] applys the function f to the col number of the pivot col
 * in the row eschelon matrix [m]
 * requires: m has been reduced to row echelon form*)
let piv_col m f g pinit ninit=
  let rows = Array.length m in
  let cols = Array.length (m.(0)) in
  let rr = row_echelon m in
    let rec trav_diag i j pacc nacc =
      if i < rows && j < cols then
        if non_zero (m.(i).(j)) then
          trav_diag (i+1) (j+1) (f i j pacc) (g i j nacc)
        else trav_diag (i) (j+1) pacc nacc
      else (pacc, nacc)
    in trav_diag 0 0 pinit ninit

let negate v =
  match v with
  | I(i) -> I (minus_big_int i)
  | F(f) -> F(-1. *. f)


let rec rem v l =
  match l with
  | [] -> []
  | h::t -> if h = v then t else h::(rem v t)

let rec from i n acc =
  if i = n then acc else (from (i+1) n (i::acc) )

let rec check_consitant m i =
  if i < 0 then true else
    let rows = Array.length m in
      if (non_zero m.(i).(rows-1)) then check_consitant m (i -1)
      else
        if Array.exists (non_zero) (m.(i)) then check_consitant m (i-1)
        else false


(*[read_off_sol m] for a matrix that is in augmented form and has one singular
 * solution this reads off the solution*)
let read_off_sol m =
  let cols = Array.length (m.(0)) in
  let (piv, non_piv) = piv_col m (fun i j acc -> (i,j)::acc) (fun _ j acc -> rem j acc ) [] (from 0 (cols) [])in
  let z,o =
        match m.(0).(0) with
        | I _ -> (I (big_int_of_int 0)),(I (big_int_of_int 1))
        | F _ -> (F 0.), F(1.)
  in
  let result = Array.make_matrix (Array.length m.(0)-1) ((List.length non_piv)) z in
    print_string (string_of_matrix m);
    List.fold_right (fun (i,j) _ -> print_string "*"; print_int i; print_string ","; print_int j; print_string "*") piv ();
    List.fold_right (fun j _ -> print_string "$"; print_int j; print_string "$") non_piv ();
    List.iter
      (fun (i,j) -> List.iteri
        (fun n ( j') ->
          if j' = cols-1 then result.(j).(n) <- (m.(i).(j'))
          else result.(j).(n) <- negate (m.(i).(j'))
        ) non_piv
      ) piv;
    List.iteri (fun i (j) -> if j = cols-1 then () else result.(i).(j) <- o) non_piv;
    result

let solve m1 m2 =
  let rows1 = Array.length m1 in
  let rows2 = Array.length m2 in
    if rows1 = 0 || Array.length m1.(0) = 0 || rows1 = rows2 then
      let aug = Array.make_matrix rows1 ((Array.length m1.(0)) +1) (F 0.) in
        Array.iteri (fun i row -> Array.iteri (fun j (F v) -> aug.(i).(j) <- m1.(i).(j)) row) m1;
        Array.iteri (fun i row -> aug.(i).(Array.length m1.(0)) <- row.(0)) m2;
          let M(sol) = red_row_echelon aug in
            if check_consitant sol ((Array.length sol) -1) then M(read_off_sol sol)
            else E("this system is not consitiant")
    else
     E "matrix size issue"

let rank m =
  N(I(big_int_of_int (fst(piv_col m (fun _ _ acc -> 1 + acc) (fun _ _ _ -> ()) 0 ()))))

let null_space m =
  let z =
        match m.(0).(0) with
        | I _ -> (I (big_int_of_int 0))
        | F _ -> (F 0.)
  in
  let M(arr) = solve m (Array.make_matrix (Array.length m) (1) (z)) in
    if (Array.length arr.(0)) > 1 then
      M(init_matrix (Array.length arr)  (Array.length arr.(0)) (fun i j -> arr.(i).(j+1)))
    else
      M(arr)

let col_space m =
  let M(rr) = row_echelon m in
  let piv = Array.of_list (fst (piv_col m (fun _ j acc -> j::acc) (fun _ _ _ -> ()) [] () )) in
    M(init_matrix (Array.length rr) (Array.length piv) (fun i j -> m.(i).(piv.(j))))

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
