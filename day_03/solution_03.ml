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

let number = Re.(compile @@ rep1 digit)

module Part_1 = struct
  let line_numbers line = Re.all number line
  let is_symbol c = not Char.(equal '.' c || ('0' <= c && c <= '9'))

  let is_valid lines (line_index, number) =
    let lines = Array.of_list lines in
    let line_length = String.length lines.(0) in
    let num_start = Re.Group.start number 0 in
    let num_end = Re.Group.stop number 0 in
    let start_index = Int.max 0 (num_start - 1) in
    let end_index = Int.min (line_length - 1) num_end in
    let has_symbol_in_range line start_index end_index =
      Iter.(start_index -- end_index)
      |> Iter.map (fun i -> String.get line i)
      |> Iter.filter is_symbol |> Iter.is_empty
    in
    let has_symbol_above =
      if Int.equal line_index 0 then false
      else
        let line_above = lines.(line_index - 1) in
        not (has_symbol_in_range line_above start_index end_index)
    in
    let has_symbol_next =
      is_symbol (String.get lines.(line_index) start_index)
      || is_symbol (String.get lines.(line_index) end_index)
    in
    let has_symbol_below =
      if Int.equal line_index (Array.length lines - 1) then false
      else
        let line_below = lines.(line_index + 1) in
        not (has_symbol_in_range line_below start_index end_index)
    in
    has_symbol_above || has_symbol_next || has_symbol_below

  let solve input =
    let lines = String.lines input in
    let line_numbers =
      List.flat_map_i
        ~f:(fun i line -> List.map ~f:(fun g -> (i, g)) (line_numbers line))
        lines
    in
    let valid_groups =
      List.filter ~f:(fun num -> is_valid lines num) line_numbers
    in
    let numbers =
      List.map ~f:snd valid_groups
      |> List.map ~f:(fun g -> Re.Group.get g 0)
      |> List.map ~f:int_of_string
    in
    List.fold_left ~f:Int.add ~init:0 numbers

  let%test "sample data" = Test.(run int (solve sample) ~expect:4361)
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
