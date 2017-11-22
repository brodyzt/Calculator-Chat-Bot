open Big_int
open Types

(*module ID_Map = Map.Make(String)

type env = func ID_Map.t*)


let evaluate_line s =
  let lexbuf = Lexing.from_string s in
    begin

      Lexer.read lexbuf;
      if Stack.is_empty Lexer.stack then "" else
        match Stack.top Lexer.stack with
        | S s -> s
        | N(I i) -> string_of_big_int i
        | N(F f) -> string_of_float f
        | E e -> e
        | _ -> failwith "unimplemented"
    end