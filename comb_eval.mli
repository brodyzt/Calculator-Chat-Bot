include Eval
(*[factorial n] computes the mathematical factorial of [n], if [n] is negative
 *then the result of the factorial will be an exn*)
val factorial: integer -> value

(*[combination a b] computes the mathematical formula for [a] choose [b] where
 * if either of the [a] and [b] are negative then the result
  of the factorial will be an exn*)
val combination: integer -> integer -> value

(*[partition_identical a b] counts the number of ways to partition [a]
 * identical elements into [b] catagories if [a] and [b] are negative then the
 * result of the factorial will be an exn*)
val partition_identical : integer -> integer -> value

(*[permutation a b] commputes the mathematical formula for [a]!/([a] − [b])!
 *where if [a] and [b] are negative then the absolute values are used and
 *computed if [a] and [b] are negative then the result of the factorial will
 * be an exn*)
val permutation: integer -> integer -> value
