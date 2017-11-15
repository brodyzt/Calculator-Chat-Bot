(*simple arithmetic compuations like adding/multiply/power/divide *)
open Eval
(*[add a b] is a + b if a and b are both integers, a +. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type*)
val add: number -> number -> number

(*[subtract a b] is a - b if a and b are both integers, a -. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type*)
val subtract: number -> number -> number

(*[multiply a b] is a * b if a and b are both integers, a *. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type*)
val multiply: number -> number -> number

(*[multiply a b] is a / b if a and b are both integers, a /. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, a,b are the same type
  and b != 0*)
val divide: number -> number -> number

(*[power a b] is a^b*)
val power: number -> number -> number
