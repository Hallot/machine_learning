(* Print a float matrix *)
val print_float_matrix : float array array -> unit

(* Transform an array into a list *)
val array_to_list : 'a array -> 'a list

(* Shuffle an array using the Fisher-Yates algorithm *)
val fisher_yates : 'a array -> unit

(* Import a csv file as a float matrix given the csv separator and its size *)
val import : string -> string -> int -> int -> float array array
