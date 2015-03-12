
(*
#cd "/home/pierre/Glasgow/ML/machine_learning/";;
#load "str.cma";;
#load "csv.cmo";;
#load "utils.cmo";;

let import_white_wine path = Csv.import path ";" 4898 12;;
let import_red_wine path = Csv.import path ";" 1599 12;;

let test_import_white path = 
Utils.print_float_matrix (import_white_wine path);;

let test_import_red path = 
Utils.print_float_matrix (import_red_wine path);;

test_import_white "/home/pierre/Glasgow/ML/winequality-white.csv";;
test_import_red "/home/pierre/Glasgow/ML/winequality-red.csv";;
*)


(* Question 1 *)
(* Import both csv files into a float array array *)
let red = Csv.import "/home/pierre/Glasgow/ML/winequality-red.csv" ";" 1599 12;;
let white = Csv.import "/home/pierre/Glasgow/ML/winequality-white.csv" ";" 4898 12;;


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
(* Return a tuple of lists (test, training) containing a list with the value for each one *)
let split matrix perc_test =
  let mat_size = (Array.length matrix) - 1 in
  let training_list = [] in
  let test_list = [] in
  let _ = Utils.fisher_yates matrix in
  let rec aux matrix perc_test n (acc1, acc2) =
    let el = Utils.array_to_list matrix.(n) in
      if n == 0 then (acc1, el::acc2)
      else let cur = ((float_of_int n) /. float_of_int mat_size) in
          if cur > 1. -. perc_test then aux matrix perc_test (n - 1) (el::acc1, acc2)
          else aux matrix perc_test (n - 1) (acc1, el::acc2)
  in aux matrix perc_test mat_size (test_list, training_list);;

let (test, training) = split red 0.3;;


(* Question 4.b *)
