(* Print a float matrix *)
val print_float_matrix : float array array -> unit

(* Transform an array into a list *)
val array_to_list : 'a array -> 'a list

(* Shuffle an array using the Fisher-Yates algorithm *)
val fisher_yates : 'a array -> unit
