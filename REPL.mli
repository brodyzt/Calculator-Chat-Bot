(*most likely we will not be sealing this module so that it will run out main
 * but just to have specifications of what we want to implement we will
 * have them in this file*)


(* [main ()] begins the repl which will continuously promt the user to input
 * commands for the calculator, evaluating them and then printining their result
 *)
val main : unit -> unit

(*[promt_and_read ()] will promt the user to input text and then read that
 * back in and return the resulting line in the form of a string*)
val promt_and_read : unit -> string


(*[display_output s] will print out the string s to the user*)
val disply_output : string -> unit