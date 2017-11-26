open Types
open Big_int

let add a b =
  match a, b with
  | I(a), I(b) -> N(I(add_big_int a b))
  | F(a), F(b) -> N(F(a+.b))
  | _ -> E(raise (Invalid_argument "Incorrect Types"))

let subtract a b =
  match a, b with
  | I(a), I(b) -> N(I(sub_big_int a b))
  | F(a), F(b) -> N(F(a-.b))
  | _ -> E(raise (Invalid_argument "Incorrect Types"))

let multiply a b =
  match a, b with
  | I(a), I(b) -> N(I(mult_big_int a b))
  | F(a), F(b) -> N(F(a*.b))
  | _ -> E(raise (Invalid_argument "Incorrect Types"))

let divide a b =
  match a, b with
  | I(a), I(b) -> N(I(div_big_int a b))
  | F(a), F(b) -> N(F(a/.b))
  | _ -> E(raise (Invalid_argument "Incorrect Types"))

let modulus a b = N(I(mod_big_int a b))

let power a b =
  match a, b with
  | I(a), I(b) -> N(I(power_big_int_positive_big_int a b))
  | F(a), F(b) -> N(F(a**b))
  | _ -> E(raise (Invalid_argument "Incorrect Types"))

let eq a b =
  match a, b with
  | I(a), I(b) -> N(I(compare_big_int a b |> big_int_of_int))
  | F(a), F(b) -> N(I(compare a b |> big_int_of_int))
  | _ -> E(raise (Invalid_argument "Incorrect types"))
