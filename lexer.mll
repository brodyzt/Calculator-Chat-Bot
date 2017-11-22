{

open Types
(*stuff to directly copy to Lexer.ml*)
let stack = Stack.create ()

let bin_op op =
  let one = Stack.pop stack in
  let two = Stack.pop stack in
    match op with
    | "+" -> begin
      match one, two with
      | N(n1), N(n2) -> Simpl_arith.add n1 n2
      | _ -> failwith "unimplemented"
    end
    | _ -> failwith "unimplemented"
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float =  ['0'-'9']+'.'['0'-'9']*
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "+"   { Stack.push (bin_op "+") stack }
  | "*" { ()}
  | "("   { ()}
  | ")"   { ()}
  | "="   {() }
  | id    { Stack.push (S (Lexing.lexeme lexbuf)) stack }
  | int   { Stack.push (N(I (Big_int.big_int_of_string (Lexing.lexeme lexbuf)))) stack }
  | eof   {() }