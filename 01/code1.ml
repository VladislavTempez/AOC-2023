let rec lines_from_file f =
  try
    let next_line = input_line f in
    next_line::(lines_from_file f)
  with
  |End_of_file -> let _ = close_in f in []
let rec digit_list_from_line l n =
  match n with
  |(-1) -> []
  |i when l.[i] >= '0' && l.[i] <= '9' ->
    ((int_of_char l.[i]) - (int_of_char '0')) :: (digit_list_from_line l (i-1))
  |i -> digit_list_from_line l (i-1)

let rec get_last_elem l =
  match l with
  |[] -> failwith "Empty list"
  |[e] -> e
  |h::q -> get_last_elem q
let num_from_line l =
  let digit_list = digit_list_from_line l ((String.length l) -1) in
  match digit_list with
  |[] -> failwith "no digit"
  |[d1] -> d1 * 11
  |h::q -> d1 + 10*(get_last_elem q)
  |_ -> failwith "too many enough digits"
let rec sum_numbers_from_lines lines =
  match lines with
  |[] -> 0
  |l::next_lines -> num_from_line l + sum_numbers_from_lines next_lines
let l = let file = open_in "false_input.txt" in (lines_from_file file)
let _ = num_from_line (List.nth l 2)
let file = open_in "false_input.txt" in
let answer = sum_numbers_from_lines (lines_from_file file) in
  Printf.printf "%d\n" answer
let _ = close_in file
