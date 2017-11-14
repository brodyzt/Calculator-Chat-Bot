(*linear algebra compuations like dot product, cross product, etc. *)

type matrix

val dot_product : matrix -> matrix -> matrix

val cross_product : matrix -> matrix -> matrix

val scale: matrix -> number -> matrix

val inverse: matrix -> matrix

val transpose: matrix -> matrix

val add: matrix -> matrix -> matrix

val subtract: matrix -> matrix -> matrix