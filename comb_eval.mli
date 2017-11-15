
(*[factorial n] computes the matmatical factorial of [n], if n is negative then
 * the factorial will be the negated factorial of the absolute value of [n]*)
val factorial: integer -> integer

(*[combination a b] commputes the matmatical formula for [a] choose [b] where
 * if [a] and [b] are negative then the absolute values are used and computed*)
val combination: integer -> integer -> integer

(*[permutation a b] commputes the matmatical formula for [a] ! ( [a] − [b] ) !
 *where if [a] and [b] are negative then the absolute values are used and
 *computed*)
val permutation: integer -> integer -> integer