(* Implements various functions on matrices for floats *)
(* Most implementations are naives and assume that arguments are valid *)


(* Return a matrix which is the product of the two matrices passed as argument *)
(* Must be of the correct size, undefined result otherwise *)
val mult_mat : float array array -> float array array -> float array array

(* Return the matrix multiplied by the scalar passed as argument *) 
val mult_mat_const : float array array -> float -> float array array

(* Transpose the matrix *)
val transpose_mat : float array array -> float array array

(* Inverse the matrix using Gauss-Jordan method *)
(* Matrix must be squared and inversible, result is undefined otherwise *)
val inverse_mat : float array array -> float array array
