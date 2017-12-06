open Big_int
open Types

(*[string_of_number n] converts a the number [n] to a string*)
let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> begin
    (*so that -0. will be displayed as 0. not -0.*)
    if f = 0. then "0." else string_of_float f
  end
  | Q (a, b) ->
    if eq_big_int b unit_big_int then string_of_big_int a
    else (string_of_big_int a )^ "/" ^ (string_of_big_int b)


(*[string_of_matrix m] converts the matrix [m] to a string*)
let string_of_matrix m =
  "[\n"^(Array.fold_right ( fun  e ac ->
    "[ "^(Array.fold_right (fun  el acc -> (string_of_number el)^" "^acc) e ("]\n"^ac))  )
    m
    "]"
     )

(*[parse_arg str] will parse the parameters contained in the string up to the
 * "->" with white spaces in between the names of the parameters into a list of
 * parameters*)
let rec parse_arg str =
  let s = String.trim str in
  if String.index s '-' = 0 then [] else
    let c = String.index s ' ' in
    let arg = String.sub s 0 c in
      arg::(parse_arg (String.sub s (c+1) ( (String.length s) -c-1 )) )

(*[parse_marco env s] adds the function represented by [s] to the enviroment
 * functions will be of the form {name : arg1 arg2 ... -> definition }*)
let parse_macro env s =
  (*name is up to the : characture*)
  let c = String.index s ':' in
  let name = String.trim (String.sub s 1 (c-1) ) in
  (*the list of arguments*)
  let args = parse_arg (String.sub s (c+1) ( (String.length s) - c -1)) in
  (*the body of the function begins after the -> and ends at the }*)
  let b = (String.index s '-') + 2 in
  let e = (String.index s '}') in
  let fun_string = String.sub s (b) (e - b) in
    ("The macro "^name^" has been successfully created",
     PMap.add name (Func(env,args,fun_string)) env)

(*[string_of_value v] converts [v] to a string*)
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
  | PrivKey (d, p, q) -> begin
    "d: "^(string_of_big_int d)^
    " p: "^(string_of_big_int p)^
    " q: "^(string_of_big_int q)
  end
  | Fact f -> begin
    List.fold_left
      (fun acc (i,c) -> acc^"("^string_of_big_int i^","
                        ^string_of_big_int c^") ")
      "" f
  end
  | P (l, r) -> "("^string_of_value l^","^string_of_value r^")"
  | Func _ -> "op"

(*[evaluate_line env s] evaluates [s] representing a program in the enviroment
 * [env] which binds all the previously defined user functions, the program can
 * define a new function contained in curly braces or evaluate a stack program
 * a new function definition will add a binging to the enviroment while
 * evlauating a program will not change the enviroment, but the top element on
 * the stack will be converte to a string and paired with the enviroment*)
let evaluate_line env s =
  try
    let lexbuf = Lexing.from_string s in
      begin
        (*adds the user defined function to the enviroment*)
        if String.length s > 0 && String.get s 0 = '{'
        then parse_macro env s else
          (*parses a stack program, returning the string form of the top
          * element on the resulting stack*)
          let env' = Lexer.read env lexbuf in
          if Stack.is_empty Lexer.stack then " ", env' else
            (string_of_value (Stack.pop Lexer.stack), env')
      end
    with _ -> (
      ("I do not understand", env)
    )
