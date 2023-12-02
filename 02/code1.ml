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

let rec reveal_is_ok red_limit green_limit blue_limit reveal =
  match reveal with
  |[] -> true
  |(count, color)::_ when color = "red" && count > red_limit -> false
  |(count, color)::_ when color = "green" && count > green_limit -> false
  |(count, color)::_ when color = "blue" && count > blue_limit -> false
  |_::next_colors -> reveal_is_ok red_limit green_limit blue_limit next_colors


let rec check_game red_limit green_limit blue_limit game_reveals game_id =
  (*return game_id if one reveal does not fit red_limit green_limit or blue_limit, 0 otherwise*)
  match game_reveals with
  |[] -> game_id
  |reveal::next_reveals when reveal_is_ok red_limit green_limit blue_limit reveal ->
    check_game red_limit green_limit blue_limit next_reveals game_id
  |_ -> let _ = Printf.printf "%d\n%!" game_id in 0

let file = open_in "example_input1.txt" in
let lines = lines_from_file file in
let (game_id, game_reveals) = process_line (List.hd lines) in
check_game 12 13 14 game_reveals game_id

let rec count_unfit_games red_limit green_limit blue_limit lines =
  match lines with
  |[] -> 0
  |current_line::next_lines ->
    let (game_id, game_reveals) = process_line (current_line) in
    (check_game red_limit green_limit blue_limit game_reveals game_id)
    + (count_unfit_games red_limit green_limit blue_limit next_lines)
let file = open_in "example_input1.txt" in
let lines = lines_from_file file in
count_unfit_games 12 13 14 lines

let file = open_in "input1.txt" in
let lines = lines_from_file file in
count_unfit_games 12 13 14 lines
