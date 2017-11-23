{

open Types
(*stuff to directly copy to Lexer.ml*)
let stack = Stack.create ()

let no_op op =
  match op with
  | "generate_private_key" -> Rsa.gen_private_key ()
  | _ -> E("not a defined operator")

let un_op op =
  if Stack.length stack < 1 then E("wrong number of arguments") else
    let one = Stack.pop stack in
      match op, one with
      | "inv" , M(m) -> Linear_alg.inverse m
      | "transpose", M(m) -> Linear_alg.transpose m
      | "echelon", M(m) -> Linear_alg.row_echelon m
      | "reduce", M(m) -> Linear_alg.red_row_echelon m
      | "det", M(m) -> Linear_alg.determinant m
      | "indep", M(m) -> Linear_alg.lin_ind m
      | "nullspace", M(m) -> Linear_alg.null_space m
      | "colspace", M(m) -> Linear_alg.col_space m
      | "!", N(I(i)) -> Comb_eval.factorial i
      | "factor", N(I(i)) -> Mod_arith.factor i
      | "gen_prime", N(I(i)) -> Mod_arith.gen_prime i
      | "is_prime", N(I(i)) -> Mod_arith.is_prime i
      | "is_prime_prob", N(I(i)) -> Mod_arith.is_prime_likely i
      | "totient", N(I(i)) -> Mod_arith.totient i
      | _, E(e) -> E(e)
      | _ -> E("not a defined operator")

let bin_op op =
  if Stack.length stack < 2 then E("wrong number of arguments") else
    let one = Stack.pop stack in
    let two = Stack.pop stack in
      match op, one, two with
      | "+", N(n1), N(n2) -> Simpl_arith.add n2 n1
      | "+", M(m1), M(m2) -> Linear_alg.add m2 m1
      | "-", N(n1), N(n2) -> Simpl_arith.subtract n2 n1
      | "-", M(m1), M(m2) -> Linear_alg.subtract m2 m1
      | "*", N(n1), N(n2) -> Simpl_arith.multiply n2 n1
      | ".", M(m1), M(m2) -> Linear_alg.dot_product m2 m1
      | "#", M(m1), M(m2) -> Linear_alg.cross_product m2 m1
      | "/", N(n1), N(n2) -> Simpl_arith.divide n2 n1
      | "^", N(n1), N(n2) -> Simpl_arith.power n2 n1
      | "%", N(I(i1)), N((I i2)) -> Mod_arith.modulus i2 i1
      | "=", N(n1), N(n2) -> Simpl_arith.eq n2 n1
      | "=", M(m1), M(m2) -> Linear_alg.eq m2 m1
      | "gcd", N(I(i1)), N((I i2)) -> Mod_arith.gcd i2 i1
      | "lcm", N(I(i1)), N((I i2)) -> Mod_arith.lcm i2 i1
      | "square", N(I(i1)), N((I i2)) -> Systems_eqs.is_square i2 i1
      | "choose", N(I(i1)), N((I i2)) -> Comb_eval.combination i2 i1
      | "perm", N(I(i1)), N((I i2)) -> Comb_eval.permutation i2 i1
      | "part", N(I(i1)), N((I i2)) -> Comb_eval.partition_identical i2 i1
      | "matrix_solve", M(m1), M(m2) -> Linear_alg.solve m2 m1
      | _, _, E(e) -> E(e)
      | _, E(e), _ -> E(e)
      | _ -> E("not a defined operator")

let tri_op op =
  if Stack.length stack < 3 then E("wrong number of arguments") else
    let one = Stack.pop stack in
    let two = Stack.pop stack in
    let three = Stack.pop stack in
      match op, one, two, three with
      | "?", N(n1), N(n2), N(n3) -> failwith "unimplemented"
      | "+~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.add i3 i2 i1
      | "-~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.subtract i3 i2 i1
      | "*~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.multiply i3 i2 i1
      | "/~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.divide i3 i2 i1
      | "^~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.power i3 i2 i1
      | "=~",  N(I(i1)), N((I i2)), N(I(i3)) -> Mod_arith.subtract i3 i2 i1
      | "crack",  N(I(i1)), N((I i2)), N(I(i3)) -> Rsa.crack (i2,i3) i1
      | "public_key", N(I(e)), N(I(q)), N(I(p)) -> Rsa.get_public_key (p,q,e)
      | "encrypt", N(I(e)), N(I(n)), S(s) -> Rsa.encrypt (n,e) s
      | _, _,_,E(e) -> E(e)
      | _, _,E(e),_ -> E(e)
      | _, E(e),_,_ -> E(e)
      | _ -> E("not a defined operator")

let quad_op op =
  if Stack.length stack < 4 then E("wrong number of arguments") else
    let one = Stack.pop stack in
    let two = Stack.pop stack in
    let three = Stack.pop stack in
    let four = Stack.pop stack in
      match op, one, two, three, four with
      | "decrypt", N(I(d)), N(I(q)), N(I(p)), N(I(i)) -> Rsa.decrypt (p,q,d) i
      | _, _,_,_,E(e) -> E(e)
      | _, _,_,E(e),_ -> E(e)
      | _, _,E(e),_,_ -> E(e)
      | _, E(e),_,_,_ -> E(e)
      | _ -> E("not a defined operator")

let rec get_n n =
  if Stack.is_empty stack || n = 0 then [] else
    (Stack.pop stack)::(get_n (n-1))

let rec pair = function
  | [] -> ([],[])
  | (N(I a))::(N (I n))::t -> begin
    let (a', n') = pair t in
      (a::a', n::n')
  end
let apply f lst =
  let l = List.rev lst in
    if List.exists (fun v -> match v with | E(e) -> true | _ -> false) l then
       List.find (fun v -> match v with | E(e) -> true | _ -> false) l
    else
      let (a,n) = pair l in
        f a n


let multi_op op =
  if Stack.length stack < 1 then E("wrong number of arguments") else
    let N(I(n)) = Stack.pop stack in
    let a = get_n (2* (Big_int.int_of_big_int n)) in
      match op with
      (*needs to check for *)
      | "solve" -> apply (Systems_eqs.crt) a
      | _ -> E("not a defined operator")

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float =  '-'? digit+'.'digit*
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let nop = "generate_private_key"
let uop = "inv" | "transpose" | "echelon" | "reduce" | "det" | "indep"
          | "nullspace" | "colspace" | "!" | "factor" | "gen_prime"
          | "is_prime" | "is_prime_prob" | "totient"
let bop = "+" | "-" | "*" | "/" | "^" | "%" | "=" | "gcd" | "lcm" | "square"
          | "choose" | "perm" | "part" | "." | "#" | "matrix_solve"
let top = "?" | "+~" | "-~" | "*~" | "/~" | "^~" | "=~" | "crack" | "public_key"
let qop = "encrypt" | "decrypt"
let mop = "solve"

rule read =
  parse
  | white { read lexbuf }
  | bop   { Stack.push (bin_op (Lexing.lexeme lexbuf)) stack; read lexbuf }
  | id { Stack.push (S (Lexing.lexeme lexbuf)) stack; read lexbuf }
  | int { Stack.push (N(I (Big_int.big_int_of_string (Lexing.lexeme lexbuf)))) stack; read lexbuf }
  | eof {()}
