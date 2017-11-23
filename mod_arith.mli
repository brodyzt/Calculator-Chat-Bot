(*arithmatic with congruence classes*)

(*[add a b n] is the unique 0 <=r < n such that (a + b - r) divides n
  if n <= 0 then the result will be an excpetion value*)
val add: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[subtract a b n] is the unique 0 <= r < n such that (a - b - r) divides n
  if n <= 0 then the result will be an excpetion value*)
val subtract: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[multiply a b n] is the unique 0 <= r < n such that (a*b - r) divides n
Precondition: n > 0*)
val multiply: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[divide a b n] is the unique 0 <= r < n such that (a*b' - r) divides n, where
  if [multiply b b' n] is 1, or -1 if no such b' exists
  if n <= 0 then the result will be an exception value*)
val divide: Types.integer -> Types.integer -> Types.integer -> Types.value


(*[power a b n] is the unique 0 <= r < n such that (a^b -r) divides n if b >=0,
  instead the unique 0 <= r < n such that (a'^-b -r) divides n, where
  [multiply a' a n] is 1, or -1 if no such a' exists.
  if n <= 0 then the result will be an exception value*)
val power: Types.integer -> Types.integer -> Types.integer -> Types.value

(*[gcd a b] is the greatest natural number d such
  that d divides a and d divides b
  note: if a or b <= 0 then [gcd a b] will be the same as gcd applied
    to the absolute value of those two numbers*)
val gcd: Types.integer -> Types.integer -> Types.value

(*[lcm a v] is the smallest natural number n
  such that a divides n and v divides n*)
val lcm: Types.integer -> Types.integer -> Types.value

(*[gen_prime l] is a psuedorandomly generated natrual number n with a high
 * probability being prime that is greater than l and which given a very large
 * uper bound may take a long time*)
val gen_prime: Types.integer -> Types.value

(*[factor n] is a list of pairs of n's prime factors and their multiplicity,
  ordered from least to greatest if n is positive, and -n's prime factors
  ordered from least to greatest if n is
  negative. It is the list with the single element 0, if n=0.*)
val factor: Types.integer -> Types.value

(*[is_prime n] is t1 (ie true) if n is a prime, 0 (ie false) if n is not a prime
  Can take an excedingly long time for large numbers*)
val is_prime: Types.integer -> Types.value

(*[is_prime_prob n] This is a faster version of is_prime, but without
 * absolute certainty of result*)
val is_prime_likely: Types.integer -> Types.value

(*[totient n] is the number of units in Z mod n. In other words it is the
  result of applying the Euler totient (phi) function to n
  if n <= 0 then the result will be an excpetion value*)
val totient: Types.integer -> Types.value
