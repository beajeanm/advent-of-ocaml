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
let ( == ) = Int.equal

module Part_1 = struct
  let is_neighbour (group_row, group) (row, col) =
    let start = Re.Group.start group 0 in
    let stop = Re.Group.stop group 0 in
    (row == group_row && col == start - 1)
    || (row == group_row && col == stop)
    || (row == group_row - 1 && col >= start - 1 && col <= stop)
    || (row == group_row + 1 && col >= start - 1 && col <= stop)

  let has_a_neighbour symbols number =
    Seq.filter (fun point -> is_neighbour number point) symbols
    |> Seq.is_empty |> not

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
      List.filter ~f:(has_a_neighbour symbol_coordinates) numbers
      |> List.map ~f:snd
      |> List.map ~f:(fun group -> Re.Group.get group 0)
      |> List.map ~f:int_of_string
    in
    List.fold_left ~f:Int.add ~init:0 valid_numbers

  let%test "sample data" = Test.(run int (solve sample) ~expect:4361)
end

module Part_2 = struct
  let is_neighbour (row, col) (num_row, num_col1, num_col2) =
    abs (row - num_row) <= 1
    && ((col >= num_col1 && col <= num_col2)
       || col == num_col1 - 1
       || col == num_col2 + 1)

  let two_neigbours (star_row, star) numbers =
    let star_col = Re.Group.start star 0 in
    let neighbours =
      List.filter
        ~f:(fun (row, group) ->
          is_neighbour (star_row, star_col)
            (row, Re.Group.start group 0, Re.Group.stop group 0 - 1))
        numbers
    in
    (* Format.printf "New Star: (%i, %i)\n" star_row star_col; *)
    (* Format.printf "neighbours: %i\n" (List.length neighbours); *)
    (* List.iter *)
    (*   ~f:(fun (i, g) -> *)
    (*     Format.printf "Num: (%i, %i, %i)\n" i (Re.Group.start g 0) *)
    (*       (Re.Group.stop g 0)) *)
    (*   numbers; *)
    if List.length neighbours == 2 then neighbours else []

  let solve input =
    let lines = String.lines input in
    let numbers =
      List.flat_map_i
        ~f:(fun row line ->
          Re.all number_re line |> List.map ~f:(fun num -> (row, num)))
        lines
    in
    let stars =
      List.flat_map_i
        ~f:(fun row line ->
          Re.all star_re line |> List.map ~f:(fun group -> (row, group)))
        lines
    in
    let valid_numbers =
      List.map
        ~f:(fun star ->
          two_neigbours star numbers |> List.map ~f:snd
          |> List.map ~f:(fun group -> Re.Group.get group 0)
          |> List.map ~f:int_of_string)
        stars
      |> List.filter ~f:(Fun.negate List.is_empty)
    in
    (* Format.printf "Valid numbers: %a" (List.pp @@ List.pp Int.pp) valid_numbers; *)
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
