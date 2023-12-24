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

let r = init_matrix (read_file "exemple1.txt")

let list_extended_neighbors i j map_matrix =
  let res = ref [] in
  let _ = if map_matrix.(i).(j) = '#' then
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

let list_neighbors i j map_matrix =
  let res = ref [] in
  let _ = if map_matrix.(i).(j) = '#' then
    ()
  else
    (
      (if map_matrix.(i-1).(j) = '.' || map_matrix.(i-1).(j) = '^' then
         res := (i-1,j)::!res);
      (if map_matrix.(i+1).(j) = '.' || map_matrix.(i+1).(j) = 'v' then
         res := (i+1,j)::!res);
      (if map_matrix.(i).(j-1) = '.' || map_matrix.(i).(j-1) = '<' then
         res := (i,j-1)::!res);
      (if map_matrix.(i).(j+1) = '.' || map_matrix.(i).(j+1) = '>' then
         res := (i,j+1)::!res);
    )
  in
  !res

let rec follow_path done_matrix map_matrix path_length i j =
  (* let _ = Printf.printf "p %d %d \n" i j in *)
  if (List.length (list_extended_neighbors i j map_matrix)) > 2 then
    (
      (* let _ = Printf.printf "p_end 3 %d %d \n" i j in *)
      (path_length, i, j)
    )
  else if (List.length (list_extended_neighbors i j map_matrix)) < 2 then
    (* let _ = done_matrix.(i).(j) <- true in *)
    (
      (* let _ = Printf.printf "p_end 3 %d %d \n" i j in *)
      (path_length, i, j)
    )
  else if map_matrix.(i-1).(j) = '.' && not done_matrix.(i-1).(j) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length+1) (i-1) j
  else if map_matrix.(i+1).(j) = '.' && not done_matrix.(i+1).(j) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length +1) (i+1) j
  else if map_matrix.(i).(j-1) = '.' && not done_matrix.(i).(j-1) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length + 1) i (j-1)
  else if map_matrix.(i).(j+1) = '.' && not done_matrix.(i).(j+1) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length + 1) i (j+1)
  else if map_matrix.(i-1).(j) = '^' && not done_matrix.(i-1).(j) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length+1) (i-1) j
  else if map_matrix.(i+1).(j) = 'v' && not done_matrix.(i+1).(j) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length +1) (i+1) j
  else if map_matrix.(i).(j-1) = '<' && not done_matrix.(i).(j-1) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length + 1) i (j-1)
  else if map_matrix.(i).(j+1) = '>' && not done_matrix.(i).(j+1) then
    let _ = done_matrix.(i).(j) <- true in
    follow_path done_matrix map_matrix (path_length + 1) i (j+1)
  else
    (* let _ = Printf.printf "%c %c %c %c %d\n" *)
    (*     map_matrix.(i-1).(j) *)
    (*     map_matrix.(i).(j-1) *)
    (*     map_matrix.(i+1).(j) *)
    (*     map_matrix.(i).(j+1) *)
    (*     (List.length (list_extended_neighbors i j map_matrix)) in *)
    failwith "unexpected path"

let get_far_neighbors done_matrix map_matrix i j =
  let close_neighbors = List.filter (fun (i,j) -> not done_matrix.(i).(j)) (list_neighbors i j map_matrix) in
  let _ = done_matrix.(i).(j) <- true in
  let res = List.map (fun (i,j) -> follow_path done_matrix map_matrix 1 i j) close_neighbors in
  let _ = done_matrix.(i).(j) <- false in
  res


let traversal adj_matrix map_matrix =
  let todo_nodes = ref [(1,1)] in
  let n = Array.length (map_matrix) in
  let m = Array.length (map_matrix.(0)) in
  let done_matrix = Array.make_matrix n m false in
  while !todo_nodes <> [] do
    let (i,j) = List.hd !todo_nodes in
    let _ = todo_nodes := List.tl !todo_nodes in
    (* let _ = Printf.printf "t %d %d \n" i j in *)
    if (i,j) <> (n-2,m-2) then
      (
        (* let _ = Printf.printf "tt %d %d \n" i j in *)
        let rec process_neighbors neighbor_list =
          match neighbor_list with
          |[] -> ()
          |(dist, next_i, next_j)::q ->
            if not done_matrix.(next_i).(next_j) then
              (
                let _ = adj_matrix.(i).(j) <- (dist, next_i, next_j)::adj_matrix.(i).(j) in
                let _ = todo_nodes := (next_i, next_j)::!todo_nodes in
                process_neighbors q)
            else
              process_neighbors q
        in
        process_neighbors (get_far_neighbors done_matrix map_matrix i j)
      )
  done

let init_graph map_matrix =
  let n = Array.length map_matrix in
  let m = Array.length map_matrix.(0) in
  let adjacency_matrix = Array.make_matrix (n) (m) [] in
  let _ = traversal adjacency_matrix map_matrix in
  adjacency_matrix

let adj = init_graph r

let list_not_dead_end adjacency_matrix =
  let res = ref [] in
  let n = Array.length adjacency_matrix in
  let m = Array.length adjacency_matrix.(0) in
  for i = 0 to (n-1) do
    for j = 0 to (m-1) do
      if adjacency_matrix.(i).(j) <> [] then
        res := (i,j)::!res
    done
  done;
  !res

let clean_graph adjacency_matrix =
  let (i_end, j_end) = ((Array.length adjacency_matrix)-2, (Array.length adjacency_matrix.(0))-2) in
  let open_vertices = list_not_dead_end adjacency_matrix in
  let n = List.length open_vertices in
  let vertices_index = Array.of_list open_vertices in
  let vertices_index_with_end = Array.init (n+1)
      (fun i -> (if i < n then vertices_index.(i) else (i_end, j_end))) in
  let get_index_from_vertices i j =
    let res = ref (-1) in
    for k = 0 to n do
      if vertices_index_with_end.(k) = (i,j) then
        res := k
    done;
    !res
  in
  let neighbors = Array.make (n+1) [] in
  for k = 0 to (n-1) do
    let (i,j) = vertices_index.(k) in
    neighbors.(k) <- List.map (fun (k,i,j) -> (get_index_from_vertices i j),k)  adjacency_matrix.(i).(j)
  done;
  neighbors.(n) <- [];
  neighbors, vertices_index_with_end


let g = clean_graph adj


let get_graph filename =
  let matrix_map = init_matrix (read_file filename) in
  let adj_matrix = init_graph matrix_map in
  clean_graph adj_matrix

let g,vert = get_graph "exemple1.txt"

let topo_sort g =
  let n = Array.length g in
  let bag = ref [(-1,n-2)] in
  let seen = Array.make (Array.length g) false in
  let topo_order = ref [] in
  while !bag <> [] do
    let (u,v) = List.hd !bag in
    let _ = bag := List.tl !bag in
    if (u=v) then
      (topo_order := u::!topo_order;
      )
    else if not seen.(v) then
      (
        bag := (v,v)::!bag;
        seen.(v) <- true;
        List.iter (fun (w,_) -> bag := (v,w)::!bag) g.(v)
      )
  done;
  List.rev (!topo_order)

let _ = topo_sort g

let rec longest_path_aux g memo topo_order =
  match topo_order with
  |[] -> memo.((Array.length memo) - 2)
  |h::q -> let _ = memo.(h) <- List.fold_left (fun max_length (index,d) -> max max_length (memo.(index) + d)) 0 g.(h)
                 in longest_path_aux g memo q

let get_longest_path g =
  let memo = Array.make (Array.length g) 0 in
  let topo_order = topo_sort g in
  2+longest_path_aux g memo topo_order

let _ = get_longest_path g
