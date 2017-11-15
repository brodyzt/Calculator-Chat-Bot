(*integers will be big integers so that the values never overflow*)
type integer = Big_int

(*a number in the language is either an integer or a floating point number*)
type number = I of integer | F of float

(*this is the type of a function in the languge*)
type function

(*this type will represent an exception in the language*)
type exn

(*the values of the language will be either strings, numbers or matricies*)
type value = S of string | N of number | M of Linear_alg.matrix | E of exn;

(*this is the enviment with all of the function bindings*)
type env

(*this will be the type for the stack that we will use to store the values
 *on*)
type stack

(*[evaluate_line s] evaluates the string [s] to a value and then gives back a
 * string form of the value*)
val evaluate_line : string -> string