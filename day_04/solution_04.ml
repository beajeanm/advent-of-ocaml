open ContainersLabels

let sample =
  {|
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}
  |> String.trim

let parser =
  let open Angstrom in
  let open Util.Parser in
  let prefix = string "Card" <* ws <* integer <* char ':' <* ws in
  let numbers = sep_by ws integer in
  prefix *> both (numbers <* string " |" <* ws) numbers

module Set = Set.Make (Int)

let filter_winnings (winning_numbers, numbers_you_have) =
  let winning_numbers =
    List.fold_left ~f:(Fun.flip Set.add) ~init:Set.empty winning_numbers
  in
  List.filter ~f:(Fun.flip Set.mem @@ winning_numbers) numbers_you_have

module Part_1 = struct
  let solve input =
    let parse line =
      Angstrom.parse_string ~consume:All parser line |> Result.get_exn
    in
    let lines = String.lines input in
    let cards = List.map ~f:parse lines in
    let winning_by_card = List.map ~f:filter_winnings cards in
    let score_by_card =
      List.filter ~f:(Fun.negate List.is_empty) winning_by_card
      |> List.map ~f:(fun l -> Int.pow 2 (List.length l - 1))
    in
    Util.sum score_by_card

  let%test "sample data" = Test.(run int (solve sample) ~expect:13)
end

module Part_2 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
