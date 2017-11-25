open Big_int
open Types


let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> string_of_float f

let string_of_matrix m =
  "[\n"^(Array.fold_left ( fun ac e ->
    "[ "^(Array.fold_left (fun acc el -> (string_of_number el)^" "^acc) ("]\n"^ac) e)  )
    "]"
     m)

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
      if String.length s > 0 && String.get s 0 = '{' then (" ",parse_macro env s) else
      (Lexer.read env lexbuf;
      if Stack.is_empty Lexer.stack then " ", env else
        match Stack.pop Lexer.stack with
        | S s -> (Stack.clear Lexer.stack); s, env
        | N n -> (Stack.clear Lexer.stack); string_of_number n, env
        | M m -> (Stack.clear Lexer.stack); string_of_matrix m, env
        | E e -> (Stack.clear Lexer.stack); e, env
        | _ -> failwith "unimplemented string conversion")
    end