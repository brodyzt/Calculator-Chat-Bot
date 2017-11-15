(* linear algebra compuations like dot product, cross product, etc. *)
include Eval

type matrix

(* [row a x] returns the row of [a] at index [x]
 * if [x] is not an the index of a row in [a], this
 * evaluates to an exeption value*)
val row: matrix -> number -> value

(* [col a x] returns the column of [a] at index [x]
 * Pif [x] is not an index of a column in [a], this
 * evaluates to an exeption value*)
val col: matrix -> number -> value

(* [dot_product a b] is [a]•[b]
 * if the width of [a] is not the same as height of [b] then
 * this evaluates to an exeption value*)
val dot_product : matrix -> matrix -> value

(* [cross_product a b] is [a]x[b]
 * if [a] and [b] are not three dimensional vectors then this evaluates
 * to an exeption value*)
val cross_product : matrix -> matrix -> matrix

(* [scale a b] is the [a] scaled by factor [b] *)
val scale: matrix -> number -> value

(* [inverse a] is the inverse matrix of [a]
 * if [a] is not an invertable a square matrix, then this evaluates
 * to an exception value*)
val inverse: matrix -> value

(* [transpose a] is [a] transposed *)
val transpose: matrix -> value

(* [add a b] is the sum of [a] and [b]
 * if [a] dees not have the same shape as [b] then this evaluates
 * to an exeption value *)
val add: matrix -> matrix -> value

(* [add a b] is the difference of [a] and [b]
 * if [a] does not have the same shape as [b] then this
 * evaluates to an exeption value*)
val subtract: matrix -> matrix -> value

(* [row_echelon a] is [a] transformed into row echelon form*)
val row_echelon: matrix -> value

(* [red_row_echelon a] is [a] transformed into reduced row echelon form*)
val red_row_echelon: matrix -> value

(* [solve a b] is the matrix containing the solution to the equation [A]x=[b]
 * where [a] is the coefficients for a system of equations
 * and b is a vector containing constants
 * if [a] does not have same height as [b] or the system is inconsistant, or
* there are infinitly many soltions this evaluates to an exception value *)
val solve: matrix -> matrix -> value

(* [determinant a] is the determinant of matrix [a]
 * if [a] is not a square matrix this evaluates to an exception value*)
val determinant: matrix -> value

(* [lind_ind a] is whether [a] is linearly independent or not *)
val lin_ind: matrix -> value

(* [lind_dep a] is whether [a] is linearly dependent or not *)
val lin_dep: matrix -> value

(* [null_space a] is the matrix containing the basis for the null space of [a] *)
val null_space: matrix -> value

(* [null_space a] is the matrix containing the basis for the col space of [a] *)
val col_space: matrix -> value
