open ContainersLabels
open Seq.Infix

let sample =
  {|
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
|}
  |> String.trim

let expanded_universe =
  {|
....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#.......
|}
  |> String.trim

type universe = {
  empty_col : int list;
  empty_row : int list;
  planets : (int * int) list;
}

let parse input =
  String.lines_seq input
  |> Seq.map (fun line -> String.to_seq line |> Seq.to_array)
  |> Seq.to_array

let equal_universe = Array.equal (Array.equal Char.equal)

let list_planets universe =
  Array.foldi
    ~f:(fun acc row_index ->
      Array.foldi
        ~f:(fun acc col_index c ->
          if Char.equal '#' c then (row_index, col_index) :: acc else acc)
        ~init:acc)
    ~init:[] universe

let create_universe grid =
  let is_empty_row = Array.for_all ~f:(Char.equal '.') in
  let is_empty_col col =
    0 --^ Array.length grid
    |> Seq.for_all (fun row -> Char.equal '.' grid.(row).(col))
  in
  let planets = list_planets grid in
  let empty_row =
    Array.mapi ~f:(fun i row -> if is_empty_row row then Some i else None) grid
    |> Array.to_list |> List.keep_some
  in
  let empty_col =
    0 --^ Array.length grid.(0)
    |> Seq.map (fun col -> if is_empty_col col then Some col else None)
    |> Seq.to_list |> List.keep_some
  in
  { planets; empty_row; empty_col }

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let distance expansion_factor universe p1 p2 =
  let points_in_between a b xs =
    let lower = min a b in
    let upper = max a b in
    List.filter ~f:(fun x -> x > lower && x < upper) xs |> List.length
  in
  let manhattan_distance = manhattan_distance p1 p2 in
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  let empty_cols = points_in_between y1 y2 universe.empty_col in
  let empty_rows = points_in_between x1 x2 universe.empty_row in
  manhattan_distance - empty_rows - empty_cols
  + ((empty_cols + empty_rows) * expansion_factor)

let%test "correct number of planets" =
  let planets = list_planets (parse sample) in
  Test.(run int (List.length planets) ~expect:9)

let%test "correct coordinates for planets" =
  let planets = list_planets (parse sample) in
  let has_first_planets =
    List.mem ~eq:(fun (a, b) (c, d) -> a = c && b = d) (0, 3) planets
  in
  Test.(run bool has_first_planets ~expect:true)

let solve_with_factor expansion_factor input =
  let universe = parse input |> create_universe in
  let double_distances =
    List.flat_map
      ~f:(fun p1 ->
        List.map
          ~f:(fun p2 -> distance expansion_factor universe p1 p2)
          universe.planets)
      universe.planets
  in
  Util.sum double_distances / 2

module Part_1 = struct
  let solve = solve_with_factor 2
  let%test "sample data" = Test.(run int (solve sample) ~expect:374)
end

module Part_2 = struct
  let solve = solve_with_factor 1_000_000

  let%test "sample data 100x" =
    Test.(run int (solve_with_factor 100 sample) ~expect:8410)

  let%test "sample data 10x" =
    Test.(run int (solve_with_factor 10 sample) ~expect:1030)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
