
(*
#cd "/home/pierre/Glasgow/ML/machine_learning/";;
#load "str.cma";;
#load "matrix.cmo";;
#load "utils.cmo";;
*)


(* Question 1 *)
(* Import both csv files into a float array array *)
let red = Utils.import "/home/pierre/Glasgow/ML/winequality-red.csv" ";" 1599 12;;
let white = Utils.import "/home/pierre/Glasgow/ML/winequality-white.csv" ";" 4898 12;;


(* Question 2 *)
(* Generate the text files that will create the graphs using gnuplot *)
(* Format is "i labeli value\n i+1...." *)
(* Generated using gnuplot with the commands:
   set boxwidth 0.5
   set style fill solid
   plot "data_red.dat" using 1:3:xtic(2) with boxes 
*)
let count_by_quality matrix =
  let size_x = (Array.length matrix - 1) in
  let res = Array.make 10 0 in
    for i = 0 to size_x do
      let index = int_of_float matrix.(i).(11) in
        res.(index) <- res.(index) + 1;
    done;
    res;;

let write_to_file file arr = 
  try 
    let oc = open_out file in
      for i = 0 to 9 do
        Printf.fprintf oc "%d %s %d\n" i ("Quality" ^ (string_of_int (i + 1))) arr.(i);
      done;
      close_out oc;
  with
    | e -> raise e;;

let generate_bar_data data file = write_to_file file (count_by_quality data);;

generate_bar_data red "data_red.dat";;
generate_bar_data white "data_white.dat";;


(* Question 4.a *)
(* Randomly split the data: 70% training 30% test *)
(* Return a tuple of arrays (test, training) containing an array with the value for each one *)
let split matrix perc_test =
  let mat_size = (Array.length matrix) in
  let num_testing = int_of_float ((float_of_int mat_size) *. perc_test) in
  let num_training = mat_size - num_testing in
  let testing_matrix = Array.make num_testing [|0.|] in
  let training_matrix = Array.make num_training [|0.|] in
  (* Shuffle the matrix *)
  let _ = Utils.fisher_yates matrix in
  (* Separate the values *)
  let rec aux matrix n (acc1, acc2) =
    let el = matrix.(n) in
    let _ =
      (* Take the num_test firsts elements and put them the testing matrix *)
      if n < num_testing then 
        let _ = acc1.(n) <- el in
          ()
      else 
        (* Put the remainder in the training matrix *)
        let _ = acc2.(n - num_testing) <- el in
          () in
      if n == mat_size - 1 then (acc1, acc2)
      else aux matrix (n + 1) (acc1, acc2)
  in aux matrix 0 (testing_matrix, training_matrix);;

let (test, training) = split red 0.3;;


(* Question 4.b *)
(* Fit a linear regressin to the model *)
(* Return the parameters of the linear model *)
let make_mat_x_t mat = 
  let m = Matrix.nb_line mat in
  let n = Matrix.nb_col mat in
  let x = Array.make_matrix m n 1. in
  let t = Array.make_matrix m 1 0. in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        if j < n - 1 then x.(i).(j + 1) <- mat.(i).(j)
        else t.(i).(0) <- mat.(i).(j)
      done
    done;
    x, t;;  

let linear_regression mat =
  let x, t = make_mat_x_t mat in
  let x_trans = Matrix.transpose x in
  let inv_xtrans_x = Matrix.inverse (Matrix.mult x_trans x) in
  let w = Matrix.mult (Matrix.mult inv_xtrans_x x_trans) t in
    w;;

let w_red = linear_regression training;; 


(* Question 4.b *)
(* Fit a linear regressin to the model *)
(* Return the parameters of the linear model *)
