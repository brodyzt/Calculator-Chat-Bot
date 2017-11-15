(*solving systems of eq with modular arith*)
open Eval

(*[bezout x y c] is a pair (Some a, Some b) where x*a + y*b = c, or (None,None)
  if no such pair exists*)
val bezout: integer -> integer -> integer -> (integer * integer) option

(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
val crt: integer list -> integer list -> (integer * integer)

(*[is_square a n] is Some true if x^2 = a (mod n) for some x,
  Some false if x^2 != a (mod n) for any x, and is None if the computation
  takes too long*)
val is_square: integer -> integer -> Some boolean
