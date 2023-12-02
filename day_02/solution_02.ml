open ContainersLabels

type die = Blue of int | Red of int | Green of int
type subset = { dice : die list }
type game = int * subset list

module Parser = struct
  open Angstrom

  let is_ws = function ' ' | '\t' -> true | _ -> false
  let ws = skip_while is_ws
  let is_int = function '0' .. '9' -> true | _ -> false
  let integer = take_while1 is_int >>| int_of_string
  let game_id = string "Game " *> integer <* char ':'

  let die =
    string "blue" *> return `Blue
    <|> string "red" *> return `Red
    <|> string "green" *> return `Green

  let die_count =
    let* count = integer <* ws in
    let+ die = die <* ws in
    match die with
    | `Blue -> Blue count
    | `Red -> Red count
    | `Green -> Green count

  let set =
    let+ dice = sep_by (char ',' *> ws) die_count in
    { dice }

  let game =
    let* game_id = game_id <* ws in
    let+ sets = sep_by (char ';' *> ws) set in
    (game_id, sets)
end

let sample =
  {|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|}
  |> String.trim

module Part_1 = struct
  let possible_die_count ~blue ~red ~green = function
    | Blue c -> c <= blue
    | Red c -> c <= red
    | Green c -> c <= green

  let possible_game ~blue ~red ~green (_, sets) =
    let possible_sets subset =
      List.for_all ~f:(possible_die_count ~blue ~red ~green) subset.dice
    in
    List.for_all ~f:possible_sets sets

  let solve input =
    let lines = String.lines input in
    let games =
      List.map ~f:(Angstrom.parse_string ~consume:All Parser.game) lines
      |> List.map ~f:Result.get_exn
    in
    let possible_games =
      List.filter ~f:(possible_game ~blue:14 ~red:12 ~green:13) games
    in
    let possible_game_ids = List.map ~f:fst possible_games in
    List.fold_left ~f:Int.add ~init:0 possible_game_ids

  let%test "sample data" = Test.(run int (solve sample) ~expect:8)
end

module Part_2 = struct
  type min_bag = { blue : int; red : int; green : int }

  let combine bag1 bag2 =
    {
      blue = max bag1.blue bag2.blue;
      red = max bag1.red bag2.red;
      green = max bag1.green bag2.green;
    }

  let min_bag_die = function
    | Blue c -> { blue = c; red = 0; green = 0 }
    | Red c -> { blue = 0; red = c; green = 0 }
    | Green c -> { blue = 0; red = 0; green = c }

  let zero = { blue = 0; red = 0; green = 0 }

  let min_bag_subset subset =
    List.map ~f:min_bag_die subset.dice |> List.fold_left ~f:combine ~init:zero

  let power bag = bag.blue * bag.red * bag.green

  let solve input =
    let lines = String.lines input in
    let games =
      List.map ~f:(Angstrom.parse_string ~consume:All Parser.game) lines
      |> List.map ~f:Result.get_exn
    in
    let bags =
      List.map ~f:(fun (_, subsets) -> List.map ~f:min_bag_subset subsets) games
      |> List.map ~f:(List.fold_left ~f:combine ~init:zero)
    in
    let powers = List.map ~f:power bags in
    List.fold_left ~f:Int.add ~init:0 powers

  let%test "sample data" = Test.(run int (solve sample) ~expect:2286)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
