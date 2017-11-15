(*solving systems of eq with modular arith*)
include Eval


(*[bezout x y c] is a pair (a, b) where x*a + y*b = c, or an exeption value
  if no such pair exists*)
val bezout: integer -> integer -> integer -> value

(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
val crt: integer list -> integer list -> value

(*[is_square a n] is 1 if x^2 = a (mod n) for some x,
  0 if x^2 != a (mod n) for any x*)
val is_square: integer -> integer -> value
