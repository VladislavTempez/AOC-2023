let rec lines_from_file f =
  try
    let next_line = input_line f in
    next_line::(lines_from_file f)
  with
  |End_of_file -> let _ = close_in f in []


let rec get_digit_here l i n digit_list =
  match digit_list with
  |[] -> None
  |(word, value)::next_possible_digits when let d = String.length word in
((i + d) <= n) && String.sub l i d = word -> Some(value)
  |(word, value)::next_possible_digits ->
    (* let _  = Printf.printf "%d %d %d %s" i (String.length word) n in *)
    get_digit_here l i n next_possible_digits

let get_standard_digits l i =
  if l.[i] > '0' && l.[i] <= '9' then
    Some ((int_of_char l.[i]) - (int_of_char '0'))
  else
    get_digit_here l i (String.length l) [("one", 1); ("two", 2); ("three", 3);
                                          ("four", 4); ("five", 5); ("six", 6);
                                          ("seven", 7); ("eight", 8); ("nine", 9)]

let rec digit_list_from_line l n =
  match n with
  |(-1) -> []
  |i -> (match get_standard_digits l i with
      |Some(j) -> j::(digit_list_from_line l (i-1))
      |None -> digit_list_from_line l (i-1)
    )
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
  |d1::q -> d1 + 10*(get_last_elem q)

let rec sum_numbers_from_lines lines =
  match lines with
  |[] -> 0
  |l::next_lines -> num_from_line l + sum_numbers_from_lines next_lines
(* let l = let file = open_in "false_input2.txt" in lines_from_file file *)
(* let _ = num_from_line (List.hd l) *)
(* let _ = digit_list_from_line (List.hd l) ((String.length (List.hd l)) -1) *)
(* let _ = get_standard_digits (List.hd l) 4 *)

let file = open_in "input2.txt" in
let answer = sum_numbers_from_lines (lines_from_file file) in
  Printf.printf "%d\n" answer
let _ = close_in file
