let import_wine path = Csv.import path ";" 1599 12;;

let test_import path = 
  Utils.print_float_matrix (import_wine path);;

test_import"/home/pierre/Glasgow/ML/winequality-red.csv";;
