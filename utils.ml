let print_matrix print_func matrix = 
  let size_x = (Array.length matrix - 1) in
  let size_y = (Array.length matrix.(0) - 1) in
    for i = 0 to size_x do
      for j = 0 to size_y do
        print_func matrix.(i).(j);
        print_string " ";
      done;
      print_newline ();
    done;;

let print_float_matrix matrix = print_matrix print_float matrix;;
