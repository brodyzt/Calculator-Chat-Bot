open Types

let add a b n =
  if (n < 0) then E("cannot take the remainder mod a negative number")
  else N(I((a + b) % n))

let subtract a b n =
  if (n < 0) then E("cannot take the remainder mod a negative number")
  else N(I((a - b) % n))

let multiply a b n =
  if (n < 0) then (E "cannot take the remainder mod a negative number")
  else N(I((a * b) % n))

let divide a b n = N(I a)


let power a b n = N(I a)


let rec gcd a b =
  if (a < 0) then gcd (-a) b
  else if (b < 0) then gcd a (-b)
  else if a < b then gcd b a
  else if (b == 0) then N(I(a))
  else gcd b (a mod b)

let lcm a b =
  N(I(a * b / (gcd a b)))

let gen_prime l = N(I l)

let factor n = N(I n)

let is_prime n = N(I n)

let is_prime_likely n = N(I n)

let totient n = N(I n)
