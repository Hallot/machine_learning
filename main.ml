(*
  let import_white_wine path = Csv.import path ";" 4898 12;;
  let import_red_wine path = Csv.import path ";" 1599 12;;

  let test_import_white path = 
  Utils.print_float_matrix (import_white_wine path);;

  let test_import_red path = 
  Utils.print_float_matrix (import_red_wine path);;

  test_import_white "/home/pierre/Glasgow/ML/winequality-white.csv";;
test_import_red "/home/pierre/Glasgow/ML/winequality-red.csv";;
*)

let red = Csv.import "/home/pierre/Glasgow/ML/winequality-red.csv" ";" 1599 12;;
let white = Csv.import "/home/pierre/Glasgow/ML/winequality-white.csv" ";" 4898 12;;
