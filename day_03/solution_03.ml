open ContainersLabels

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

let is_neighbour (group_row, group) (row, col) =
  let ( == ) = Int.equal in
  let start = Re.Group.start group 0 in
  let stop = Re.Group.stop group 0 in
  (row == group_row && col == start - 1)
  || (row == group_row && col == stop)
  || (row == group_row - 1 && col >= start - 1 && col <= stop)
  || (row == group_row + 1 && col >= start - 1 && col <= stop)

let has_neighbours coordinates element =
  Seq.filter (fun point -> is_neighbour element point) coordinates
  |> Seq.is_empty |> not

module Part_1 = struct
  let solve input =
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
    let symbol_coordinates =
      List.map ~f:(fun (row, group) -> (row, Re.Group.start group 0)) symbols
      |> Seq.of_list
    in
    let valid_numbers =
      List.filter ~f:(has_neighbours symbol_coordinates) numbers
      |> List.map ~f:snd
      |> List.map ~f:(fun group -> Re.Group.get group 0)
      |> List.map ~f:int_of_string
    in
    List.fold_left ~f:Int.add ~init:0 valid_numbers

  let%test "sample data" = Test.(run int (solve sample) ~expect:4361)
end

module Part_2 = struct
  let solve input = 0
  (* let%test "sample data" = Test.(run int (solve sample) ~expect:467835) *)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
