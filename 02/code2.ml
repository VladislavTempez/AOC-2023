let rec lines_from_file f =
  try
    let next_line = input_line f in
    next_line::(lines_from_file f)
  with
  |End_of_file -> let _ = close_in f in []

let process_game_reveal s =
  let comma_split = String.split_on_char ',' s in
  List.map (fun s_color_couple -> Scanf.sscanf s_color_couple " %d %s" (fun x y -> (x,y))) comma_split

let process_line l =
  let colon_split = String.split_on_char ':' l in
  let game_id = Scanf.sscanf (List.hd colon_split) "Game %d" (fun x -> x) in
  let game_reveals_string = List.hd (List.tl colon_split) in
  let game_reveals_list = String.split_on_char ';' game_reveals_string in
  (game_id, List.map (process_game_reveal) game_reveals_list)

let file = open_in "example_input1.txt" in
let lines = lines_from_file file in
process_line (List.hd lines)

let rec min_limits red_limit green_limit blue_limit reveal =
  match reveal with
  |[] -> (red_limit, green_limit, blue_limit)
  |(count, color)::next_colors when color = "red" && count > red_limit ->
    min_limits count green_limit blue_limit next_colors
  |(count, color)::next_colors when color = "green" && count > green_limit ->
    min_limits red_limit count blue_limit next_colors
  |(count, color)::next_colors when color = "blue" && count > blue_limit ->
    min_limits red_limit green_limit count next_colors
  |_::next_colors -> min_limits red_limit green_limit blue_limit next_colors

let rec compute_game_power red_limit green_limit blue_limit game_reveals =
  (*return game_id if one reveal does not fit red_limit green_limit or blue_limit, 0 otherwise*)
  match game_reveals with
  |[] -> red_limit * green_limit * blue_limit
  |reveal::next_reveals ->
    let (new_r_limit, new_g_limit, new_b_limit) = min_limits red_limit green_limit blue_limit reveal in
    compute_game_power new_r_limit new_g_limit new_b_limit next_reveals

let file = open_in "example_input2.txt" in
let lines = lines_from_file file in
let (game_id, game_reveals) = process_line (List.hd lines) in
compute_game_power 0 0 0 game_reveals

let rec sum_powers lines =
  match lines with
  |[] -> 0
  |current_line::next_lines ->
    let (_, game_reveals) = process_line (current_line) in
    (compute_game_power 0 0 0 game_reveals)
    + (sum_powers next_lines)
let file = open_in "example_input2.txt" in
let lines = lines_from_file file in
sum_powers lines

let file = open_in "input2.txt" in
let lines = lines_from_file file in
sum_powers lines
