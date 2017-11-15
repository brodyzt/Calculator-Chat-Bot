(*simple arithmetic compuations like adding/multiply/power/divide *)
include Eval
(*[add a b] is a + b if a and b are both integers, a +. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type,
  if they are not the result will be an excpetion value*)
val add: number -> number -> value

(*[subtract a b] is a - b if a and b are both integers, a -. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type
  if they are not the result will be an excpetion value*)
val subtract: number -> number -> value

(*[multiply a b] is a * b if a and b are both integers, a *. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type
  if they are not the result will be an excpetion value*)
val multiply: number -> number -> value

(*[multiply a b] is a / b if a and b are both integers, a /. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, a,b are the same type
  and b != 0, if they are not the result will be an excpetion value*)
val divide: number -> number -> value

(*[power a b] is a^b, warning on floats this value may overflow*)
val power: number -> number -> value
