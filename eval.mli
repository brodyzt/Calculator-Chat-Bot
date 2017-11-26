(*[evaluate_line env s] evaluates the stack program [s] in the enviroment
 * [env] with the resulting value being the pair of the resulting enviroment
 * and the string form of the evaluation of the program*)
val evaluate_line: Types.env -> string -> (string * Types.env)