open Big_int
open Types

(*module ID_Map = Map.Make(String)

type env = func ID_Map.t*)


let evaluate_line s =
  let line = Lexing.from_string s in
    Lexer.read line;
    match Stack.pop Lexer.stack with
    | S s -> s
    | N(I i) -> string_of_big_int i
    | N(F f) -> string_of_float f
    | _ -> failwith "unimplemented"