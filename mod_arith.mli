(*arithmatic with congruence classes*)
open Eval
(*[add a b n] is the unique 0 <=r < n such that (a + b - r) divides n
  Precondition: n > 0*)
val add: integer -> integer -> integer -> integer

(*[subtract a b n] is the unique 0 <= r < n such that (a - b - r) divides n
Precondition: n > 0*)
val subtract: integer -> integer -> integer -> integer

(*[multiply a b n] is the unique 0 <= r < n such that (a*b - r) divides n
Precondition: n > 0*)
val multiply: integer -> integer -> integer -> integer

(*[divide a b n] is the unique 0 <= r < n such that (a*b' - r) divides n, where
  if [multiply b b' n] is 1, or -1 if no such b' exists
  Precondition: n > 0*)
val divide: integer -> integer -> integer -> integer


(*[power a b n] is the unique 0 <= r < n such that (a^b -r) divides n if b >=0,
  instead the unique 0 <= r < n such that (a'^-b -r) divides n, where
  [multiply a' a n] is 1, or -1 if no such a' exists.
  Precondition: n > 0*)
val power: integer -> integer -> integer -> integer

(*[gcd a b] is the greatest natural number d such
  that d divides a and d divides b*)
val gcd: integer -> integer -> integer

(*[lcm a v] is the smallest natural number n
  such that a divides n and v divides n*)
val lcm: integer -> integer -> integer

(*[gen_prime] is a randomly generated natrual number n, and a float f
  such that the probability that n is a prime is not less than f*)
val gen_prime: unit -> integer*float

(*[factor n] is a list of n's prime factors, ordered from least to greatest if
  n is positive, and -n's prime factors ordered from least to greatest if n is
  negative. It is the list with the single element 0, if n=0.*)
val factor: integer -> integer list

(*[is_prime n] is true if n is a prime, false if n is not a prime
  Can take an excedingly long time for large numbers*)
val is_prime: integer -> boolean

(*[is_prime_prob n] is a float p, where the probability that n is prime is not
  less than p, or is -1 if n is certainly composite. This is a faster version
  of is_prime, but without certainty of result*)
val is_prime_prob: integer -> float

(*[totient n] is the number of units in Z mod n. In other words it is the
  result of applying the Euler totient (phi) function to n*)
val totient: integer -> integer
