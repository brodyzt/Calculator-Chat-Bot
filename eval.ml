open Big_int
open Types


let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> string_of_float f

let string_of_matrix m =
  "[\n"^(Array.fold_right ( fun e ac ->
    "[ "^(Array.fold_right (fun el acc -> (string_of_number el)^" "^acc) e ("]\n"^ac))  )
    m
    "]")

let rec parse_arg s =
  if String.index s '-' = 0 then [] else
    let c = String.index s ' ' in
    let arg = String.sub s 0 c in
      arg::(parse_arg (String.sub s (c+1) ( (String.length s) -c-1 )) )

let parse_macro env s =
  let c = String.index s ':' in
  let name = String.sub s 0 c in
  let args = parse_arg s in
  let b = (String.index s '-') + 2 in
  let fun_string = String.sub s (b) ((String.length s) - b) in
    (PMap.add name (Func(env,args,fun_string)) env)

let evaluate_line env s =
  let lexbuf = Lexing.from_string s in
    begin
      if String.get s 0 = '{' then ("",parse_macro env s) else
      (Lexer.read env lexbuf;
      if Stack.is_empty Lexer.stack then "", env else
        match Stack.top Lexer.stack with
        | S s -> s, env
        | N n -> string_of_number n, env
        | M m -> string_of_matrix m, env
        | E e -> e, env
        | _ -> failwith "unimplemented")
    end