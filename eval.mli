(*sends the ast form of the command to the proper module to evaluate it*)
type integer = Big_int

type number = I of integer | F of float

type matrix = unit;

type function = unit

type value = S of string | N of number | M of matrix;
