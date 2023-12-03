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

let example_lines = read_file "example_input1.txt"

let rec extract_numbers (s:string) (line_number:int) (current_position:int) (starting_index:int) (current_number:int) :(int*int*int*int) list =
  match current_position with
  |k when k = String.length s && starting_index = k -> []
  |k when k = String.length s -> (current_number, line_number,  starting_index,k-1)::[]
  |k when (s.[k] < '0' || s.[k] > '9') && starting_index = k -> extract_numbers s line_number (k +1) (k+1) 0
  |k when s.[k] < '0' || s.[k] > '9' -> (current_number, line_number, starting_index, k-1)::(extract_numbers s line_number (k +1) (k+1) 0)
  |k -> extract_numbers s line_number (k +1) (starting_index) (current_number*10 + ((int_of_char s.[k]) - (int_of_char '0')))
  
let _ = extract_numbers (List.hd example_lines) 0 0 0 0

let rec extract_symbols (s:string) (line_number:int) (current_position:int) :(char*int*int) list =
  match current_position with
  |k when k = String.length s -> []
  |k when (s.[k] < '0' || s.[k] > '9') && s.[k] <> '.' -> (s.[k], line_number, current_position)::(extract_symbols s line_number (k+1))
  |k -> extract_symbols s line_number (k+1)

let _ = extract_symbols (List.hd (List.tl example_lines)) 1 0

let rec extract_all_numbers (lines:string list) (current_line:int)=
  match lines with
  |[] -> []
  |head::tail -> (extract_numbers (head) (current_line) 0 0 0)@(extract_all_numbers tail (current_line +1))

let _ = extract_all_numbers example_lines 0


let rec extract_all_symbols (lines:string list) (current_line:int)=
  match lines with
  |[] -> []
  |head::tail -> (extract_symbols (head) (current_line) 0)@(extract_all_symbols tail (current_line +1))

let example_symbols = extract_all_symbols example_lines 0


let rec number_adjacent_to_symbol (line:int) (starting_column:int) (ending_column:int) (symbol_positions:(char*int*int) list) :bool =
  match symbol_positions with
  |[] -> false
  |(_,l,c)::_ when ((l = line) || (l = line +1) || (l = line -1)) && (c >= starting_column -1 && c <= ending_column+1) -> true
  |_::t -> number_adjacent_to_symbol line starting_column ending_column t

let _ = number_adjacent_to_symbol 0 5 7 example_symbols


let sum_part_numbers (name:string) :int =
  let lines = read_file name in
  let number_positions = extract_all_numbers lines 0 in
  let symbol_positions = extract_all_symbols lines 0 in
  let fold_fun (acc:int) (n:int*int*int*int)  :int =
    let (number, line, col_s, col_e) = n in
    if number_adjacent_to_symbol line col_s col_e symbol_positions then
      number + acc
    else
      acc
  in
  List.fold_left (fold_fun) 0 number_positions
  
let _ = sum_part_numbers "example_input1.txt"

let _ = sum_part_numbers "input1.txt"

let rec gear_ratio (line:int) (column:int) (number_positions) (current_gear_ratio:int) (number_of_numbers:int)=
  match number_positions with
  |[] when number_of_numbers = 2 -> current_gear_ratio
  |[] -> 0
  |(number, n_l, col_s, col_e)::tail when
         (line >= n_l -1 && line <= n_l +1) && (column >= col_s -1 && column <= col_e + 1) -> gear_ratio line column tail (current_gear_ratio * number) (number_of_numbers + 1)
  |_::tail -> gear_ratio line column tail current_gear_ratio number_of_numbers

let sum_gear_ratio (name:string) :int =
  let lines = read_file name in
  let number_positions = extract_all_numbers lines 0 in
  let symbol_positions = extract_all_symbols lines 0 in
  let fold_fun (acc:int) (n:char*int*int)  :int =
    let (c, line, col) = n in
    if c = '*' then
      (gear_ratio line col number_positions 1 0) + acc
    else
      acc
  in
  List.fold_left (fold_fun) 0 symbol_positions

let _ = sum_gear_ratio "example_input1.txt"

let _ = sum_gear_ratio "input1.txt"
