(* linear algebra compuations like dot product, cross product, etc. *)

type matrix

(* [row a x] returns the row of [a] at index [x]
 * Precondition: [x] is the index of a row in [a]
 * raises: [Error] if row [x] does not exist *)
val row: matrix -> number -> matrix

(* [col a x] returns the column of [a] at index [x]
 * Precondition: [x] is the index of a column in [a]
 * raises: [Error] if column [x] does not exist *)
val col: matrix -> number -> matrix

(* [dot_product a b] is [a]•[b]
 * Precondition: width of [a] is same as height of [b] 
 * raises: [Error] if width of [a] is not same as height of [b] *)
val dot_product : matrix -> matrix -> matrix

(* [cross_product a b] is [a]x[b]
 * Precondition: [a] and [b] are three dimensional vectors 
 * raises: [Error] if either matrix is not a three dimensional vector *)
val cross_product : matrix -> matrix -> matrix

(* [scale a b] is the [a] scaled by factor [b] *)
val scale: matrix -> number -> matrix

(* [inverse a] is the inverse matrix of [a]
 * Precondition: [a] is a square matrix
 * raises: [Error] if [a] is not a square matrix *)
val inverse: matrix -> matrix

(* [transpose a] is [a] transposed *)
val transpose: matrix -> matrix

(* [add a b] is the sum of [a] and [b]
 * Precondition: [a] has the same shape as [b]
 * raises: [Error] if [a] does not have same shape as [b] *)
val add: matrix -> matrix -> matrix

(* [add a b] is the difference of [a] and [b]
 * Precondition: [a] has the same shape as [b]
 * raises: [Error] if [a] does not have same shape as [b] *)
val subtract: matrix -> matrix -> matrix

(* [row_echelon a] is [a] transformed into row echelon form*)
val row_echelon: matrix -> matrix

(* [red_row_echelon a] is [a] transformed into reduced row echelon form*)
val red_row_echelon: matrix -> matrix

(* [solve a b] is the matrix containing the solution to the equation [A]x=[b]
 * where [a] is the coefficients for a system of equations
 * and b is a vector containing constants 
 * Precondition: [a] has same height as [b] 
 * Raises: [Error] if [a] has different height than [b], if system is inconsistent, 
 * or if there are infinitely many solutions *)
val solve: matrix -> matrix -> matrix

(* [determinant a] is the determinant of matrix [a] 
 * Precondition: [a] is a square matrix
 * raises: [Error] if [a] is not a square matrix *)
val determinant: matrix -> number

(* [lind_ind a] is whether [a] is linearly independent or not *)
val lin_ind: matrix -> bool

(* [lind_dep a] is whether [a] is linearly dependent or not *)
val lin_dep: matrix -> bool

(* [null_space a] is the matrix containing the basis for the null space of [a] *)
val null_space: matrix -> matrix
