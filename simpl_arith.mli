(*simple arithmetic compuations like adding/multiply/power/divide *)

(*[add a b] is a + b if a and b are both integers, a +. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type,
  if they are not the result will be an excpetion value*)
val add: Eval.number -> Eval.number -> Eval.value

(*[subtract a b] is a - b if a and b are both integers, a -. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type
  if they are not the result will be an excpetion value*)
val subtract: Eval.number -> Eval.number -> Eval.value

(*[multiply a b] is a * b if a and b are both integers, a *. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, and are the same type
  if they are not the result will be an excpetion value*)
val multiply: Eval.number -> Eval.number -> Eval.value

(*[multiply a b] is a / b if a and b are both integers, a /. b if a and b are
  both floats
  Precondition: a,b are both either integers or floats, a,b are the same type
  and b != 0, if they are not the result will be an excpetion value*)
val divide: Eval.number -> Eval.number -> Eval.value

(*[power a b] is a^b, warning on floats this value may overflow*)
val power: Eval.number -> Eval.number -> Eval.value
