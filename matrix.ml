let nb_line mat = Array.length mat;;
let nb_col mat = Array.length mat.(0);;

let mult mat1 mat2 = 
  let m1 = nb_line mat1 in
  let m2 = nb_line mat2 in
  let n2 = if m2 == 0 then 0 else nb_col mat2 in
  let res = Array.make_matrix m1 n2 0. in
    for i = 0 to m1 - 1 do
      for j = 0 to n2 - 1 do
        for k = 0 to m2 - 1 do
          res.(i).(j) <- res.(i).(j) +. mat1.(i).(k) *. mat2.(k).(j)
        done
      done
    done;
    res;;


let transpose mat = 
  let m = nb_line mat in
  let n = nb_col mat in
  let res = Array.make_matrix n m 0. in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        res.(i).(j) <- mat.(j).(i)
      done
    done;
    res;;


let mult_const mat const =
  let m = nb_line mat in
  let n = nb_col mat in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        mat.(i).(j) <- const *. mat.(i).(j)
      done
    done;
    mat;;


let combine_lines mat (l_combined, coeff1) (l_to_add, coeff2) =
  let n = nb_col mat in
    for j = 0 to n - 1 do
      mat.(l_combined).(j) <- (coeff1 *. mat.(l_combined).(j)) +. (coeff2 *. mat.(l_to_add).(j))
    done;;


let pivot mat inv (a, b) =
  let n = nb_col mat in
  let piv = mat.(a).(b) in
    for j = a + 1 to n - 1 do
      combine_lines inv (j, piv) (a, 0. -. mat.(j).(b));
      combine_lines mat (j, piv) (a, 0. -. mat.(j).(b))
    done;;


let find_piv mat j = 
  let n = nb_col mat in
  let k = ref j in
    while (!k < n - 1 && mat.(!k).(j) = 0.) do
      k := !k + 1
    done;
    !k;;


let swap_el mat (a, b) (c, d) = 
  let temp = mat.(a).(b) in
    mat.(a).(b) <- mat.(c).(d);
    mat.(c).(d) <- temp;;


let swap_lines mat i j =
  let n = nb_col mat in
    for k = 0 to n - 1 do
      swap_el mat (i, k) (j, k)
    done;;


let pivot_iter mat inv j =
  let piv_index = find_piv mat j in
    if piv_index <> j then
      begin
        swap_lines mat j piv_index;
        swap_lines inv j piv_index;
      end;
    pivot mat inv (j, j);;


let reduce_diag mat inv = 
  let m = nb_line mat in
  let n = nb_col mat in
    for i = 0 to m - 1 do
      let piv = mat.(i).(i) in
        if piv <> 1. then 
          if piv <> 0. then
            begin
              for j = 0 to n - 1 do
                mat.(i).(j) <- mat.(i).(j) /. piv;
                inv.(i).(j) <- inv.(i).(j) /. piv;
              done;
            end;
    done;;


let rec n_compose f el n =
  if n = 0 then el
  else n_compose f (f el) (n - 1);;


let make_inv_mat m n = 
  let mat = Array.make_matrix m n 0. in
    for i = 0 to (min (m - 1) (n - 1)) do
      mat.(i).(i) <- 1.
    done;
    mat;;


let rotate mat = 
  let m = nb_line mat in
  let n = nb_col mat in
  let res = Array.make_matrix n m 0. in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        res.(i).(j) <- mat.(m - i - 1).(n - j - 1)
      done
    done;
    res;;


let reduce_rotate (mat1, mat2) =
  let m = nb_line mat1 in
    for i = 0 to m - 2 do
      pivot_iter mat1 mat2 i
    done;
    (rotate mat1, rotate mat2);;


let inverse mat =
  let temp = mat in
  let m = nb_line mat in
  let inv = make_inv_mat m m in
  let (res1, res2) = n_compose reduce_rotate (temp, inv) 2 in
    reduce_diag res1 res2;
    res2;;
