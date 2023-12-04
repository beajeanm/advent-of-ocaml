open ContainersLabels
module Group = Re.Group

let sample =
  {|
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|}
  |> String.trim

let number_re = Re.(compile @@ rep1 digit)
let star_re = Re.(compile @@ char '*')
let symbol_re = Re.(compile @@ diff any (alt [ char '.'; digit ]))
let ( == ) = Int.equal

let numbers_from_groups groups =
  List.map ~f:snd groups
  |> List.map ~f:(fun group -> Group.get group 0)
  |> List.map ~f:int_of_string

(** Check if a number associated with its line number is a neighbour of a symbol represented by a dot *)
let is_neighbour (num_row, num) (row, col) =
  let num_col1 = Group.start num 0 in
  let num_col2 = Group.stop num 0 - 1 in
  abs (row - num_row) <= 1
  && ((col >= num_col1 && col <= num_col2)
     || col == num_col1 - 1
     || col == num_col2 + 1)

(** Parse the input and return its numbers and the symbols according to symbol_re *)
let numbers_and_symbol input symbol_re =
  let lines = String.lines input in
  let numbers =
    List.flat_map_i
      ~f:(fun row line ->
        Re.all number_re line |> List.map ~f:(fun num -> (row, num)))
      lines
  in
  let symbols =
    List.flat_map_i
      ~f:(fun row line ->
        Re.all symbol_re line |> List.map ~f:(fun group -> (row, group)))
      lines
  in
  (numbers, symbols)

module Part_1 = struct
  (** Check if this number has at least 1 neighbour *)
  let has_a_neighbour symbols number =
    List.filter ~f:(fun point -> is_neighbour number point) symbols
    |> List.is_empty |> not

  let solve input =
    let numbers, symbols = numbers_and_symbol input symbol_re in
    let symbol_coordinates =
      List.map ~f:(fun (row, group) -> (row, Group.start group 0)) symbols
    in
    let valid_numbers =
      List.filter ~f:(has_a_neighbour symbol_coordinates) numbers
      |> numbers_from_groups
    in
    List.fold_left ~f:Int.add ~init:0 valid_numbers

  let%test "sample data" = Test.(run int (solve sample) ~expect:4361)
end

module Part_2 = struct
  (** Return the number adjacent of a symbol, if the symbols has exactly 2 adjacent numbers.
      Return the empty list otherwise. *)
  let two_neighbours (star_row, star) numbers =
    let star_col = Group.start star 0 in
    let neighbours =
      List.filter ~f:(Fun.flip is_neighbour (star_row, star_col)) numbers
    in
    if List.length neighbours == 2 then neighbours else []

  let solve input =
    let numbers, stars = numbers_and_symbol input star_re in
    let valid_numbers =
      List.map
        ~f:(fun star -> two_neighbours star numbers |> numbers_from_groups)
        stars
      (* We need to remove the empty lists to avoid adding 1 for each empty list to the sum*)
      |> List.filter ~f:(Fun.negate List.is_empty)
    in
    List.map ~f:(List.fold_left ~f:Int.mul ~init:1) valid_numbers
    |> List.fold_left ~f:Int.add ~init:0

  let%test "sample data" = Test.(run int (solve sample) ~expect:467835)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
