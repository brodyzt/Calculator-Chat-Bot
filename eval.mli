(*[evaluate_line env s] evaluates [s] representing a program in the enviroment
 * [env] which binds all the previously defined user functions, the program can
 * define a new function contained in curly braces or evaluate a stack program
 * a new function definition will add a binging to the enviroment while
 * evlauating a program will not change the enviroment, but the top element on
 * the stack will be converte to a string and paired with the enviroment*)
val evaluate_line: Types.env -> string -> (string * Types.env)