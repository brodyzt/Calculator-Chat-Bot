System Description
==================
We have implemented a programmable calculator that will have capabilities in multiple 
computer science related concepts a first or second year Cornell student my need help with. 
These topics include modular arithmetic, linear algebra, combinatorics, as well as basic 
calculator functionality such as multiplication and addition. 

Discrete Help
-------------
Functionality inculding :
- Computing powers modulo a number 
- GCD 
- LCM
- factorization (of reasonable numbers) 
- computation of the totient (of reasonable numbers)
- RSA Cryptography: 
  + encryption given public key 
  + decryption given a private key 
  + small prime generation
  + probabilistic large prime generation
- Solving systems of linear congruences modulo a number (and other easy diophantine equations, 
in addition to indicating if simple quadratic equations have a solution)
- Basic Combinatorics including factorials, permutations, and combinations

Linear Algebra Help
-------------------
Simple matrix operations 
- adding
- scaling 
- dot product 
- cross product
Operations for system of linear equations including
- transforming a matrix to row (reduced) echelon form 
- solving a system of linear equations 
- calculating the inverse using row operations Calculating Determinants
- Operations on Vector spaces including indicating linear independence and 
dependence and finding the basis of the col/null space of a matrix

Programmability
---------------
Our programming language is a small mathematical stack based language written 
in postfix (RPN) notation. The calculator comes with many built in operators 
for all of the above outlined topics, and allow the user to write their own operators

Instructions for set up
========================
+ Running the Calculator in the terminal
  1. The only the packages used with the terminal version are oUint, and nums
  so you should not need to install anything to run the command line version
  2. There is a make file to make running the code simpler
    - `make test` will run the test suite
    - `make repl` will compile and run the 
    - `make clean` will clean up the files built
+ Running the server
    - `make compile` will compile the code
    - ./sever.ml will run the server

+ Using the messanger bot
    - you can message "Ocaml Calculator" or CLICK THIS LINK to message the the bot

Using the calculator
================

Types
------
+ integers (of infinite size)
  - ex. `35738589746878732509813958093850980935`
+ floats (of 64 bit size)
  - ex. `34.564`
+ strings (must be contained in quotation marks)
  - ex.   `"Hello!!"`
+ matricies (row major) and require spaces after commas and no spaces after ]
  - ex. `[[2., -3.], [-4., 5.]]`

Built in Operators
------------------
+ `a b +` will compute the normal integer/floating point/matrix addition of `a` and `b`
  where `a` and `b` are of the same types, if they are both matricies then they
  must be of the same size
+ `a b -` will compute normal integer/floating point subtraction `a` and `b`
  where `a` and `b` are of the same types, if they are both matricies then they
  must be of the same size
+ `a b *` will compute normal integer/ floating point multiplication of `a` and `b`
+ `a b /` will compute integer/floating point division of `a` `b` 
  - note: integer division will be floored for numbers where the `b` does not divide the `a`. If `b` is 0 then this equation will evaluate to 0 and a warning will be given.
+ `a b ^` operator will raise `a` to the `b` power where `a` and `b` are normal integers or floating point numbers. This may cause floating point numbers to overflow, but integers will never overflow their value, though could cause aslow response.
+ `a b =` tests equality of `a` and `b` which are normal integer or floating point values
+ `a b c ?` will be used like an if statement. `a` will act as the guard, so if it evaluates to a non zero value then the stack will process `b` and if `a` is a zero value then `c` will be processed by the stack.

+ `a b %` will compute the remainder of `a` when divided by `b` where `a` and `b` are integers, and `b` is positive	
+ `a b c +~` will compute the sum of `a` and `b` taken mod `c` where `a`, `b`, and `c` are integers, and `c` is positive	
+ `a b c -~`  will compute the difference of `a` and `b` taken mod `c` where `a`, `b`, and `c` are integers, and `c` is positive
+ `a b c *~` will compute the product of `a` and `b` taken mod `c` where `a`, `b`, and `c` are integers, and `c` is positive
+ `a b c /~` will compute the division of `a` and `b` taken mod `c` where `a`, `b`, and `c` are integers, and `c` is positive
  - note: if the `b` is not relatively prime to `c` then division cannot proceed.
+ `a b c ^~` operator will raise `a` to the `b` power then take the remainder mod `c`, where `a`, `b`, and `c` are integers, and `c` is positive	
+ `a b c =~` tests the equality of `a` and `b` mod `c`, where `a`, `b`, and `c` are integers, and `c` is positive	
+ `a b gcd` computes the greatest common divisor of `a` and `b`, where `a`, `b`, and `c` are integers
+ `a b lcm` computes the least common multiple of `a` and `b`, where `a`, `b`, and `c` are integers
+ `a factor` computes the factors of `a`, where `a` is an integer
+ `a gen_prime` is an integer greater than `a`, which is prime with very high probability\
  - ` 'prime` will give the most recently generated prime
+ `n is_prime` is 1 if `n` is prime and 0 if `n` is not prime, can be slow for large `n`
+ `n is_prime_prob` is 1 if `n` is prime and 0 if `n` is not prime with high probability, much faster than is_prime, but not guaranteed to be correct.
  - note: if the user types `'prime_prob` you will recieve the result of the last call to is_prime_prob
+ `a totient` computes the Euler’s totient function of `a` where `a` is an integer > 0
+ `p q d public_key` will generate a public key based on the private key `(p, q, d)`
  - `'n` and `'e` will give the public key most recently calculated
  - though generate_public_key takes 3 parameters, it can take the resulting tuple produced by generate_private_key
+ `generate_private_key` will generate, with very high probability, a triple (d,p,q) where p,q are two large prime numbers, and d is a unit modulo p*q
   - `'p`, `'q`, and `'d` will give the private key most recently generated
+ `a b c encrypt` will encrypt the string `a` using `b` `c` as the public key where `b` is a large number, and `d` is a unit modulo `b`.
  - though encrypt takes 3 parameters, it can take 2 the second being the resulting pair created by `public_key` 
+ `a b c d decrypt` will decrypt the number `a` using `b` `c` `d` as the private key where `b` is a large prime integer, `c` is a large prime integer, and `d` is a unit modulo the product of `b` and `c`.
  - though encrypt takes 4 parameters, it can take 2 the second being the resulting tuple produced by generate_private_key
+ `a b c crack` will attempt to decrypt the integer `c` using `a` `b` as the public key where `a` is the product of 2 large primes, and `b` is a unit mod `a`.
  - though crack takes 3 parameters, it can take 2 the second being the resulting pair created by `public_key` 
+ `a0b0 … anbn n solve` solves the system of equations x=a₀ (mod b₀) … x=an(mod bn) where the as and bs are integers, there are `n` of those equations, and they do not contradict
+ `a b square` indicates if `a` is a square mod `b` where `b` is a positive integer, and `a` is positive integer. Can take a very long time for large `b`

+ `a !` is the calculation of a factorial where `a` is a positive integer
+ `a b choose` is the calculation of `a` choose `b` where `a` and `b` are positive integer
+ `a b perm` is the calculation of the number of choices of `a` items from `b` where order matters, `a` and `b` are positive integers 
+ `a b part` is the calculation of the number of ways `a` items can be partitioned into `b` categories where all `a` items would be indistinguishable, `a` and `b` are positive integers 

+ `a b .` computes the dot product of `a` and `b` where `a` and `b` are vectors
+ `a b #` computes the cross product of `a` and `b` where `a` and `b` are vectors of height 3
+ `a b *` computes `a` scaled by `b` where `b` is a number and `a` is a matrix
+ `a inv` computes the inverse of the matrix `a` if `a` is invertable
+ `a transpose` computes the transpose of the matrix `a`
+ `a echelon` reduces `a` to echelon form where `a` is a matrix
+ `a reduce` reduces `a` to reduced echelon form where `a` is a matrix
+ `a b matrix_solve` solves the linear systems of equations [a]x=[b] where a is a matrix of size n by m and b is of size n by 1, and the system is consistent
+ `a det` computes the determinant of `a`, where `a` is a square matrix
+ `a indep` computes if the columns of `a` are linearly independent where `a` is a matrix
+ `a dep` computes if the columns of `a` are linearly dependent where `a` is a matrix
+ `a nullspace` computes a matrix with the columns of the matrix being the null space of `a` where `a` is a matrix
+ `a colspace` computes a matrix with the columns of the matrix being the column space of `a` where `a` is a matrix

Defining a New Operator
-----------------------
 
`{<name of function>: <param₁> … <paramn> -> <stack program for method>}`
Then to execute the function the user can list the arguments and then place the 
function name after the arguments listed in order
* ex. ```{ add1 : x -> x 1 + }
             3 add1```
- note₀: we do not have constant variable definitions, but if you want 
to define a variable they could create a constant function that will always 
evaluate to some value.
- note₁: we will be using lexical scope for operators.
