
#cd "/home/pierre/Glasgow/ML/machine_learning/";;
#load "str.cma";;
#load "matrix.cmo";;
#load "utils.cmo";;
(*
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
(* Fit a linear regression to the model *)
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


(* Question 4.c *)
(* Make a scatter plot of the prediction *)
(* Calculate the prediction for the test set *)
let compute_prediction train_mat test_mat =
  let x, t = make_mat_x_t test_mat in
  let w = linear_regression train_mat in
  let m = Matrix.nb_line x in
  let n = Matrix.nb_col x in
  let res = Array.make_matrix m 1 0. in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        res.(i).(0) <- res.(i).(0) +. w.(j).(0) *. x.(i).(j)
      done
    done;
    res;;

(* plot "scatterplot_red.dat"  with points pt 7 notitle *)
let write_scatterplot prediction actual file =
  let m = Matrix.nb_line prediction in
    try 
      let oc = open_out file in
        for i = 0 to m - 1 do
          Printf.fprintf oc "%f %f\n" actual.(i).(0) prediction.(i).(0);
        done;
        close_out oc;
    with
      | e -> raise e;;


let test_qualities_gen test_mat = 
  let m = Matrix.nb_line test_mat in
  let res = Array.make_matrix m 1 0. in
    for i = 0 to m - 1 do
      res.(i).(0) <- test_mat.(i).(11);
    done;
    res;;

let test_qualities = test_qualities_gen test;;
let test_predictions = compute_prediction training test;;

write_scatterplot test_predictions test_qualities "scatterplot_red.dat";;

(* Compute the mean squared error *)
let mean_square_error mat1 mat2 =
  let res = ref 0. in
  let m = Matrix.nb_line mat1 in
    for i = 0 to m - 1 do
      res := !res +. (mat1.(i).(0) -. mat2.(i).(0)) *. (mat1.(i).(0) -. mat2.(i).(0))
    done;
    !res /. float_of_int m;;


(* Question 4.e *)
(* Implement the accuracy benchmark *)
let accuracy prediction actual =
  let res = ref 0 in
  let m = Matrix.nb_line prediction in
    for i = 0 to m - 1 do
      if abs_float (prediction.(i).(0) -. actual.(i).(0)) < 0.5 then res := !res + 1
    done;
    (float_of_int !res) /. (float_of_int m);;


(* Question 5.a *)
(* Implement the regularised least squared regression *)
let least_square_regression mat =
  let x, t = make_mat_x_t mat in
  let w = linear_regression mat in
  let m = Matrix.nb_line mat in
  let t_minus_xw = Matrix.sub t (Matrix.mult x w) in
  let sigma = Matrix.mult (Matrix.transpose t_minus_xw) t_minus_xw in
    (w, sigma.(0).(0) /. float_of_int m);;


(* Add the epsilon term based on a normal distribution to the value when computing it *)
let compute_prediction_least_square train_mat test_mat sigma mu =
  let x, t = make_mat_x_t test_mat in
  let w = linear_regression train_mat in
  let m = Matrix.nb_line x in
  let n = Matrix.nb_col x in
  let res = Array.make_matrix m 1 0. in
  let pi = 4. *. atan 1. in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        res.(i).(0) <- res.(i).(0) +. (w.(j).(0) *. x.(i).(j)) 
      done;
      let epsilon = (1. /. (sqrt (sigma *. 2. *. pi))) *. exp ((1. /. (-2. *. sigma *. sigma)) *. (res.(i).(0) -. mu)) in
        res.(i).(0) <- res.(i).(0) +. epsilon;
    done;
    res;;


(* Plot the results *)
(* Run the rergssion 1000 times on new data each time *)
(* plot "perf_vs_reg.dat"  with points pt 7 notitle *)
let plot_results mat file =
  try 
    let oc = open_out file in
      for i = 0 to 999 do
        let (te, tr) = split mat 0.3 in
        let w, sigma = least_square_regression tr in
        let test_qual = test_qualities_gen te in
        let test_pred = compute_prediction_least_square tr te sigma 0. in
        let acc = accuracy test_pred test_qual in
          Printf.fprintf oc "%f %f\n" sigma acc;
      done;
      close_out oc;
  with
    | e -> raise e;;


(* plot_results red "perf_vs_reg.dat";; *)

(* Question 5.c *)
(* Implement k-fold cross-validation *)
let copy_except mat k k_fold =
  let m = Matrix.nb_line mat in
  let n = Matrix.nb_col mat in
  let res = Array.make_matrix (m - (m / k)) n 0. in
  let res2 = Array.make_matrix (m / k) n 0. in
  let skip = ref 0 in
  let c = ref 0 in
    for i = 0 to m - 1 do
      if i < (m / k) * k_fold || i >= (m / k) * (k_fold + 1) then 
        begin
          for j = 0 to n - 1 do
            if !skip = 1 then res.(i - (m / k)).(j) <- mat.(i - (m / k)).(j)
            else res.(i).(j) <- mat.(i).(j)
          done;
        end
      else 
        begin
          skip := 1;
          let _ = c := !c + 1 in
            for j = 0 to n - 1 do
              res2.(!c - 1).(j) <- mat.(!c - 1).(j);
            done;
        end
    done;
    res, res2;;


let k_cross_validation k mat =
  let n = Matrix.nb_col mat in
  let w_max = Array.make_matrix n 1 0. in
  let sigma_max = ref 0. in
  let max_acc = ref 0. in
    Utils.fisher_yates mat;
    for i = 0 to k - 1 do
      let train, test = copy_except mat k i in
      let w, sigma = least_square_regression (train) in
      let test_qual = test_qualities_gen test in
      let test_pred = compute_prediction_least_square train test sigma 0. in
      let acc = accuracy test_pred test_qual in
        if acc > !max_acc then
          begin
            for j = 0 to n - 1 do
              w_max.(j).(0) <- w.(j).(0)
            done;
            sigma_max := sigma;
            max_acc := acc;
          end;
    done;
    (w_max, !sigma_max);;


(* Question 6.d *)
(* Implement KNN *)

(* Manhattan distance *)
let manhattan point_a point_b =
  let m = Array.length point_a in
  let res = ref 0. in
    for i = 0 to m - 1 do
      res := !res +. abs_float (point_a.(i) -. point_b.(i))
    done;
    !res;;

let compare_man p1 p2 ref_point =
  compare (manhattan ref_point p1) (manhattan ref_point p2);;

(* Adapted from http://rosettacode.org/wiki/Sorting_algorithms/Heapsort#OCaml *)
(* Content is available under GNU Free Documentation License 1.2. *)
let heapsort comp a b =
  let swap i j =
    let ta = a.(i) in 
    let tb = b.(i) in
      a.(i) <- a.(j); a.(j) <- ta;
      b.(i) <- b.(j); b.(j) <- tb 
  in

  let sift k l =
    let rec check x y =
      if 2*x+1 < l then
        let ch =
          if y < l-1 && comp a.(y) a.(y+1) < 0 then y+1 else y in
          if comp a.(x) a.(ch) < 0  then (swap x ch; check ch (2*ch+1)) in
      check k (2*k+1) in

  let len = Array.length a in

    for start = (len / 2) - 1 downto 0 do
      sift start len;
    done;

    for term = len - 1 downto 1 do
      swap term 0;
      sift 0 term;
    done;;

let sort_dist ref_point matrix matrix2= 
  let comp p1 p2 = compare_man p1 p2 ref_point in
    heapsort comp matrix matrix2;;

let not_in_mat point mat = 
  let b = ref true in
  let n = Matrix.nb_line mat in
    for i = 0 to n - 1 do
      if point == mat.(i) then b := false
    done;
    (!b);;


(* Return the k nearest neighbour from a point *)
let find_k_nearest_neighbour k point matrix =
  let m = Matrix.nb_line matrix in
  let n = Matrix.nb_col matrix in
  let k_closer = Array.make_matrix k n 10000. in
  let k_id = Array.make_matrix k n 0 in
    (* Initialise the array because 0. is bad *)
    for i = 0 to k - 1 do
      if  point == matrix.(i) || (not_in_mat matrix.(i) k_closer) = false then (k_closer.(i) <- matrix.(k + i); k_id.(i).(0) <- k + i)
      else (k_closer.(i) <- matrix.(i); k_id.(i).(0) <- i)
    done;
    sort_dist point k_closer k_id;
    for i = 0 to m - 1 do
      if (manhattan point matrix.(i)) < (manhattan point k_closer.(k - 1)) && point <> matrix.(i)  && (not_in_mat matrix.(i) k_closer) then
        begin 
          k_closer.(k - 1) <- matrix.(i);
          k_id.(k - 1).(0) <- i;
          sort_dist point k_closer k_id;
        end;
    done;
    sort_dist point k_closer k_id;
    k_closer, k_id;;



let split_quality matrix =
  let m = Matrix.nb_line matrix in
  let n = Matrix.nb_col matrix in
  let dat = Array.make_matrix m (n - 1) 0. in
  let quality = Array.make_matrix m 1 0. in
    for i = 0 to m - 1 do
      for j = 0 to n - 2 do
        dat.(i).(j) <- matrix.(i).(j)
      done;
      quality.(i).(0) <- matrix.(i).(n - 1);
    done;
    dat, quality;;

let highest_class mat = 
  let m = Matrix.nb_line mat in
  let num = Array.make_matrix m 1 0 in
  let max = ref 0 in
    for i = 0 to m - 1 do
      num.(int_of_float mat.(i).(0)) <- [|num.(int_of_float mat.(i).(0)).(0) + 1|]
    done;
    for i = 0 to m - 1 do
      if num.(i).(0) > !max then max := i
    done;
    !max;;

let get_quality_from_points k_closer k_id mat = 
  let m = Matrix.nb_line k_closer in
  let n = Matrix.nb_col mat in
  let num = Array.make_matrix m 1 0. in
    for i = 0 to m - 1 do
      let id = k_id.(i).(0) in
        num.(i).(0) <- mat.(id).(n - 1);
    done;
    num;;

let dtr, qtr = split_quality training;;
let a, b = find_k_nearest_neighbour 10 dtr.(0) dtr;;
get_quality_from_points a b training;;

(* Return the computed class values as well as the actual ones *)
let k_nearest k training test =
  let dtr, qtr = split_quality training in
  let dte, qte = split_quality test in
  let m = Matrix.nb_line dte in
  let comp_equal = Array.make_matrix m 1 0. in
    for i = 0 to m - 1 do
      let point_mat, id_mat = find_k_nearest_neighbour k dte.(i) dtr in
      let class_mat = get_quality_from_points point_mat id_mat training in
      let hi_class = highest_class class_mat in
        comp_equal.(i).(0) <- float_of_int hi_class
    done;
    comp_equal, qte;;


(* Calculate the accuracy *)
let k_acc k mat1 mat2 = 
  let a, b = k_nearest k mat1 mat2 in
    accuracy a b;;

k_acc 8 training test;;



















