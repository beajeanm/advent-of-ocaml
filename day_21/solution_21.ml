open ContainersLabels

let sample =
  {|
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
|}
  |> String.trim

let parse input =
  String.lines input |> List.map ~f:String.to_array |> Array.of_list

let find_s grid =
  Grid.fold
    (fun position content s_position ->
      if Char.equal 'S' content then position else s_position)
    grid (0, 0)

let n_steps grid s_position n =
  let open Seq.Infix in
  let is_empty position = Grid.get grid position |> fun c -> Char.('#' <> c) in
  let one_step position =
    [
      Grid.north position;
      Grid.south position;
      Grid.west position;
      Grid.east position;
    ]
    |> List.filter ~f:(Grid.inside grid)
    |> List.filter ~f:is_empty
  in
  let compare_position = CCOrd.(pair int int) in
  let all_next_steps positions =
    List.flat_map ~f:one_step positions |> List.sort_uniq ~cmp:compare_position
  in
  0 --^ n
  |> Seq.fold_left (fun positions _ -> all_next_steps positions) [ s_position ]

module Part_1 = struct
  let solve_for_n n input =
    let grid = parse input in
    n_steps grid (find_s grid) n |> List.length

  let solve input = solve_for_n 64 input
  let%test "sample data" = Test.(run int (solve_for_n 6 sample) ~expect:16)
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
