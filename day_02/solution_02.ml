open ContainersLabels

type colour_dice = Blue of int | Red of int | Green of int
type subset = { handfull : colour_dice list }
type bag = { blue : int; red : int; green : int }

module Parser = struct
  open Angstrom
  open Util.Parser

  let game_id = string "Game " *> integer <* char ':'

  let die =
    string "blue" *> return `Blue
    <|> string "red" *> return `Red
    <|> string "green" *> return `Green

  let die_count =
    let* count = integer <* whitespaces in
    let+ die = die <* whitespaces in
    match die with
    | `Blue -> Blue count
    | `Red -> Red count
    | `Green -> Green count

  let set =
    let+ handfull = sep_by (char ',' *> whitespaces) die_count in
    { handfull }

  let game =
    let* game_id = game_id <* whitespaces in
    let+ sets = sep_by (char ';' *> whitespaces) set in
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
  let possible_die_count bag = function
    | Blue c -> c <= bag.blue
    | Red c -> c <= bag.red
    | Green c -> c <= bag.green

  let possible_game bag (_, sets) =
    let possible_sets subset =
      List.for_all ~f:(possible_die_count bag) subset.handfull
    in
    List.for_all ~f:possible_sets sets

  let solve input =
    let bag = { blue = 14; red = 12; green = 13 } in
    let lines = String.lines input in
    let games =
      List.map ~f:(Angstrom.parse_string ~consume:All Parser.game) lines
      |> List.map ~f:Result.get_exn
    in
    let possible_games = List.filter ~f:(possible_game bag) games in
    let possible_game_ids = List.map ~f:fst possible_games in
    Util.sum possible_game_ids

  let%test "sample data" = Test.(run int (solve sample) ~expect:8)
end

module Part_2 = struct
  let combine bag1 bag2 =
    {
      blue = max bag1.blue bag2.blue;
      red = max bag1.red bag2.red;
      green = max bag1.green bag2.green;
    }

  let bag_from_die = function
    | Blue c -> { blue = c; red = 0; green = 0 }
    | Red c -> { blue = 0; red = c; green = 0 }
    | Green c -> { blue = 0; red = 0; green = c }

  let empty_bag = { blue = 0; red = 0; green = 0 }

  let bag_of_subset subset =
    List.map ~f:bag_from_die subset.handfull
    |> List.fold_left ~f:combine ~init:empty_bag

  let power_of_bag bag = bag.blue * bag.red * bag.green

  let solve input =
    let lines = String.lines input in
    let games =
      List.map ~f:(Angstrom.parse_string ~consume:All Parser.game) lines
      |> List.map ~f:Result.get_exn
    in
    let required_bags =
      List.map ~f:(fun (_, subsets) -> List.map ~f:bag_of_subset subsets) games
      |> List.map ~f:(List.fold_left ~f:combine ~init:empty_bag)
    in
    let powers = List.map ~f:power_of_bag required_bags in
    Util.sum powers

  let%test "sample data" = Test.(run int (solve sample) ~expect:2286)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
