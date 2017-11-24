open Big_int
open Types

(*module ID_Map = Map.Make(String)

type env = func ID_Map.t*)

let string_of_number n =
  match n with
  | I i -> string_of_big_int i
  | F f -> string_of_float f

let string_of_matrix m =
  "[\n"^(Array.fold_right ( fun e ac ->
    "[ "^(Array.fold_right (fun el acc -> (string_of_number el)^" "^acc) e ("]\n"^ac))  )
    m
    "]")



let evaluate_line s =
  let lexbuf = Lexing.from_string s in
    begin

      Lexer.read lexbuf;
      if Stack.is_empty Lexer.stack then "" else
        match Stack.top Lexer.stack with
        | S s -> s
        | N n -> string_of_number n
        | M m -> string_of_matrix m
        | E e -> e
        | _ -> failwith "unimplemented"
    end