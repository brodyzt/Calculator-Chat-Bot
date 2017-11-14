(*linear algebra compuations like dot product, cross product, etc. *)

type matrix

(* [dot_product a b] is a•b where a and b are both matrices *)
val dot_product : matrix -> matrix -> matrix

(* [cross_product a b] is axb where a and b are both matrices *)
val cross_product : matrix -> matrix -> matrix

(* [scale a b] is the matrix a scaled by factor b *)
val scale: matrix -> number -> matrix

(* [inverse a] is the inverse matrix of matrix a *)
val inverse: matrix -> matrix

(* [transpose a] is matrix a transposed *)
val transpose: matrix -> matrix

(* [add a b] is the sum of matrices a and b *)
val add: matrix -> matrix -> matrix

(* [add a b] is the difference of matrices a and b *)
val subtract: matrix -> matrix -> matrix
