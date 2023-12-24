let read_file (name:string) :string list =
  let f = open_in name in
  let rec read_lines (lines:string list) :string list =
    try
      let line = input_line f in read_lines (line::lines)
    with
    |End_of_file -> List.rev lines
  in let result = read_lines []
     in close_in f;
        result

let rec string_to_list i s =
  if i = String.length s then
    []
  else
    s.[i]::(string_to_list (i+1) s)

let init_matrix line_list =
  if line_list = [] then
    failwith "empty lines"
  else
    let n = String.length (List.hd line_list) in
    let m = List.length line_list in
    let res = Array.make_matrix m n ' ' in
    let rec fill_line line_index column_index line_list =
      match line_list with
      |[] when column_index = n -> ()
      |[] -> failwith "line too short"
      |h::q -> let _ = res.(line_index).(column_index)<-h in fill_line line_index (column_index +1) q
    in
    let _  = List.iteri (fun i l -> fill_line i 0 l) (List.map (string_to_list 0) line_list) in
    let _ = res.(0).(1) <- '#' in
    let _ = res.(n-1).(m-2) <- '#' in
    res

let list_extended_neighbors i j map_matrix =
  let res = ref [] in
  let _ =
    if map_matrix.(i).(j) = '#' then
      ()
    else
      (
        (if map_matrix.(i-1).(j) <> '#' then
           res := (i-1,j)::!res);
        (if map_matrix.(i+1).(j) <> '#' then
           res := (i+1,j)::!res);
        (if map_matrix.(i).(j-1) <> '#' then
           res := (i,j-1)::!res);
        (if map_matrix.(i).(j+1) <> '#' then
           res := (i,j+1)::!res);
      )
  in
  !res

let list_valid_neighbors i j map_matrix done_matrix =
  List.filter (fun (i,j) -> not done_matrix.(i).(j)) (list_extended_neighbors i j map_matrix)

let rec backtrack_traversal map done_tile i j =
  let n = Array.length map and m = Array.length map.(0) in
  let dead_end_distance = -((n+2) * (m+2)) in
  if i = (Array.length map)-2 && j = (Array.length map) - 2 then
    0
  else
    (
      done_tile.(i).(j) <- true;
      let r = 1 + (List.fold_left (fun max_path (i_next, j_next)
                       -> max (backtrack_traversal map done_tile i_next j_next) max_path)
                     dead_end_distance
                     (list_valid_neighbors i j map done_tile))
      in
      let _ = done_tile.(i).(j) <- false in
      r
    )

let max_degree map =
  let n = Array.length map and m = Array.length map.(0) in
  let r = ref 0 in
  let n_max = ref 0 in
  let n_nodes = ref 0 in
  for i = 1 to n-2 do
    for j = 1 to m-2 do
      let d_out = (List.length (list_extended_neighbors i j map)) in
      let _ = if d_out > 2 then
        n_nodes := !n_nodes +1 in
      if d_out > !r then
        (r:= d_out;
         n_max := 0)
      else if !r = d_out then
        (
         n_max := !n_max +1
        )
      done
    done;
  !r, !n_max, !n_nodes

let _ = let m = init_matrix (read_file "input1.txt") in
max_degree m


let get_longest_path filename =
  let map = init_matrix (read_file filename) in
  let n = Array.length map and m = Array.length map.(0) in
  let done_tile = Array.make_matrix n m false in
  2+ (backtrack_traversal map done_tile 1 1)

let _ = let r = get_longest_path "exemple1.txt" in Printf.printf "%d\n%!" r
let _ = let r = get_longest_path "input1.txt" in Printf.printf "%d\n%!" r
