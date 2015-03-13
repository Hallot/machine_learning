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


let array_to_list arr =
  let rec array_to_list_aux arr n acc = 
    if n == 0 then arr.(0)::acc
    else array_to_list_aux arr (n - 1) (arr.(n)::acc)
  in array_to_list_aux arr ((Array.length arr) - 1) [];;


let swap arr i j =
  let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp;;

let fisher_yates arr = 
  let _ = Random.self_init () in
    for i = (Array.length arr - 1) downto 1 do
      swap arr i (Random.int (i + 1));
    done;;


let import file_name separator size_x size_y =
  let reg_separator = Str.regexp separator in
  let value_array = Array.make_matrix size_x size_y 0. in
  let i = ref 0 in
    try
      let ic = open_in file_name in
      (* Skip the first line, columns headers *)
      let _ = input_line ic in
        try
          while true; do
            (* Create a list of values from a line *)
            let line_list = Str.split reg_separator (input_line ic) in
            let j = ref 0 in
              List.iter (fun elem -> value_array.(!i).(!j) <- float_of_string elem; j:= !j + 1) line_list;
              i := !i + 1
          done;
          value_array
        with 
          | End_of_file -> close_in ic; value_array
    with
      | e -> raise e;;
