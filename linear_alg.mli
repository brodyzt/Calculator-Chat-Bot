(* linear algebra compuations like dot product, cross product, etc. *)

(* [row a x] returns the row of [a] at index [x]
 * if [x] is not an the index of a row in [a], this
 * evaluates to an exeption value*)
val row: Eval.matrix -> Eval.number -> Eval.value

(* [col a x] returns the column of [a] at index [x]
 * Pif [x] is not an index of a column in [a], this
 * evaluates to an exeption value*)
val col: Eval.matrix -> Eval.number -> Eval.value

(* [dot_product a b] is [a]•[b]
 * if the width of [a] is not the same as height of [b] then
 * this evaluates to an exeption value*)
val dot_product : Eval.matrix -> Eval.matrix -> Eval.value

(* [cross_product a b] is [a]x[b]
 * if [a] and [b] are not three dimensional vectors then this evaluates
 * to an exeption value*)
val cross_product : Eval.matrix -> Eval.matrix -> Eval.value

(* [scale a b] is the [a] scaled by factor [b] *)
val scale: Eval.matrix -> Eval.number -> Eval.value

(* [inverse a] is the inverse matrix of [a]
 * if [a] is not an invertable a square matrix, then this evaluates
 * to an exception value*)
val inverse: Eval.matrix -> Eval.value

(* [transpose a] is [a] transposed *)
val transpose: Eval.matrix -> Eval.value

(* [add a b] is the sum of [a] and [b]
 * if [a] dees not have the same shape as [b] then this evaluates
 * to an exeption value *)
val add: Eval.matrix -> Eval.matrix -> Eval.value

(* [add a b] is the difference of [a] and [b]
 * if [a] does not have the same shape as [b] then this
 * evaluates to an exeption value*)
val subtract: Eval.matrix -> Eval.matrix -> Eval.value

(* [row_echelon a] is [a] transformed into row echelon form*)
val row_echelon: Eval.matrix -> Eval.value

(* [red_row_echelon a] is [a] transformed into reduced row echelon form*)
val red_row_echelon: Eval.matrix -> Eval.value

(* [solve a b] is the matrix containing the solution to the equation [A]x=[b]
 * where [a] is the coefficients for a system of equations
 * and b is a vector containing constants
 * if [a] does not have same height as [b] or the system is inconsistant, or
* there are infinitly many soltions this evaluates to an exception value *)
val solve: Eval.matrix -> Eval.matrix -> Eval.value

(* [determinant a] is the determinant of matrix [a]
 * if [a] is not a square matrix this evaluates to an exception value*)
val determinant: Eval.matrix -> Eval.value

(* [lind_ind a] is whether [a] is linearly independent or not *)
val lin_ind: Eval.matrix -> Eval.value

(* [lind_dep a] is whether [a] is linearly dependent or not *)
val lin_dep: Eval.matrix -> Eval.value

(* [null_space a] is the matrix containing the basis for the null space of [a] *)
val null_space: Eval.matrix -> Eval.value

(* [null_space a] is the matrix containing the basis for the col space of [a] *)
val col_space: Eval.matrix -> Eval.value
