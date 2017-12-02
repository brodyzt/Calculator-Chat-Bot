open Big_int
open Types


let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> begin
    (*because apparently -0. is a thing*)
    if f = 0. then "0." else
      string_of_float f
  end

let string_of_matrix m =
  "[\n"^(Array.fold_right ( fun  e ac ->
    "[ "^(Array.fold_right (fun  el acc -> (string_of_number el)^" "^acc) e ("]\n"^ac))  )
    m
    "]"
     )

let rec parse_arg str =
  let s = String.trim str in
  if String.index s '-' = 0 then [] else
    let c = String.index s ' ' in
    let arg = String.sub s 0 c in
      arg::(parse_arg (String.sub s (c+1) ( (String.length s) -c-1 )) )

let parse_macro env s =
  let c = String.index s ':' in
  let name = String.trim (String.sub s 1 (c-1) ) in
  let args = parse_arg (String.sub s (c+1) ( (String.length s) - c -1)) in
  let b = (String.index s '-') + 2 in
  let e = (String.index s '}') in
  let fun_string = String.sub s (b) (e - b) in
    (PMap.add name (Func(env,args,fun_string)) env)

let rec string_of_value v =
  (Stack.clear Lexer.stack);
  match v with
  | S s ->  s
  | N n -> string_of_number n
  | M m -> string_of_matrix m
  | E e -> e
  | PubKey (n, e) -> begin
    "n: "^(string_of_big_int n)^" e: "^(string_of_big_int e)
  end
  | PrivKey (p, q, d) -> begin
    "p: "^(string_of_big_int p)^
    " q: "^(string_of_big_int q)^
    " d: "^(string_of_big_int d)
  end
  | Fact f -> begin
    List.fold_left
      (fun acc (i,c) -> acc^"("^string_of_big_int i^","
                        ^string_of_big_int c^") ")
      "" f
  end
  | P (l, r) -> "("^string_of_value l^","^string_of_value r^")"
  | Func _ -> "op"

let evaluate_line env s =
  let lexbuf = Lexing.from_string s in
    begin
      if String.length s > 0 && String.get s 0 = '{' then (" ",parse_macro env s) else
      (Lexer.read env lexbuf;
      if Stack.is_empty Lexer.stack then " ", env else
        (string_of_value (Stack.pop Lexer.stack), env))
    end
