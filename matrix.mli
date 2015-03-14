(* Implements various functions on matrices for floats *)
(* Most implementations are naives and assume that arguments are valid *)

(* Return the number of columns of a matrix *)
val nb_col : float array array -> int

(* Return the number of lines of a matrix *)
val nb_line : float array array -> int

(* Return a matrix which is the product of the two matrices passed as argument *)
(* Must be of the correct size, undefined result otherwise *)
val mult : float array array -> float array array -> float array array

(* Return the matrix multiplied by the scalar passed as argument *) 
val mult_const : float array array -> float -> float array array

(* Transpose the matrix *)
val transpose : float array array -> float array array

(* Inverse the matrix using Gauss-Jordan method *)
(* Matrix must be squared and inversible, result is undefined otherwise *)
val inverse : float array array -> float array array
