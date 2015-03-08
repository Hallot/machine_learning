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
              List.iteri
                (fun j elem -> value_array.(!i).(j) <- float_of_string elem)
                line_list;
              i := !i + 1
          done;
          value_array
        with 
          | End_of_file -> close_in ic; value_array
        with
          | e -> raise e;;
