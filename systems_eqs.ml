open Types
(*[bezout x y c] is a pair (a, b) where x*a + y*b = c, or an exeption value
  if no such pair exists*)
let bezout x y c = P(N(I x), N(I(y)))

(*[crt lst1 lst2] is a pair (a,M) such that any integer n congruent to a mod M
  satisfies n = bi (mod mi) for any 0 <= bi <= j ,
  where lst1 = [b0,b1,...,bj] and lst2 = [m0,m1,...,mj].
  Precondition: all elements of lst2 are pairwise relatively prime, and greater
  than 0, and lst1 and lst2 are of the same length*)
let crt lst1 lst2 = P(N(I(Big_int.big_int_of_int 0)), N(I(Big_int.big_int_of_int 0)))

(*[is_square a n] is 1 if x^2 = a (mod n) for some x,
  0 if x^2 != a (mod n) for any x*)
let is_square a n = N(I(Big_int.big_int_of_int 0))