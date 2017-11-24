open Array
open Big_int
type integer = Big_int.big_int

type number = I of integer | F of float



type exn = string

type matrix = number array array

(*A public_key of (n,e) is an RSA public key
  where gcd(e,n) = 1 and n = pq for some
  primes p and q*)
type public_key = integer * integer

(*A private_key of (d,p,q) is an RSA private key (d,pq) for
  the public key (e,n), where gcd(d,n) = 1
  and d satisfies e*d = 1 (mod phi(n)), where phi
  is the Euler phi function*)
type private_key = integer * integer * integer

(* a list of pairs of primes and their multiplicity for some number*)
type factors = (integer * integer) list
module PMap = Map.Make(String)

type env = value PMap.t

and func = env * string list * string

and value = S of string | N of number | M of matrix | E of exn |
             PubKey of public_key | PrivKey of private_key |
             Fact of factors | P of pair | Func of func
and pair = value * value

