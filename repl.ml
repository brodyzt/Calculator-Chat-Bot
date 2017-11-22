(*[promt_and_read ()] will promt the user to input text and then read that
 * back in and return the resulting line in the form of a string*)
let promt_and_read _ =
  print_string "\n>";
  match read_line () with
  | exception End_of_file -> ""
  | file_name -> file_name

(*[display_output s] will print out the string s to the user*)
let display_output s = print_string s

let rec loop _ =
  let s = promt_and_read () in
  let result = Eval.evaluate_line s in
    display_output result; loop ()


(* [main ()] begins the repl which will continuously promt the user to input
 * commands for the calculator, evaluating them and then printining their result
 *)
let main _ =
  (print_string ("\n\n**Welcome to the CS helper buddy**"^
              "\nWe can help with, combinatorics, number theory,"^
              " and linear algebra, you are also welcome to make"^
              " your own macros to extend the functionality");
  (loop ()))



let () = main ()