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

let parse input =
  String.lines_seq input
  |> Seq.map (fun line -> String.to_seq line |> Seq.to_array)
  |> Seq.to_array

let equal_universe = Array.equal (Array.equal Char.equal)

(* The most inneficient array manipulation in the universe, but it will get the job done. *)
let expand universe =
  let empty_row = Array.for_all ~f:(Char.equal '.') in
  let empty_col universe col =
    0 --^ Array.length universe
    |> Seq.for_all (fun row -> Char.equal '.' universe.(row).(col))
  in
  let add_universe_col universe col =
    let add_row_col row =
      let new_lenght = Array.length row + 1 in
      let new_row = Array.init new_lenght ~f:(Fun.const '.') in
      0 -- col |> Seq.iter (fun i -> new_row.(i) <- row.(i));
      col --^ Array.length row |> Seq.iter (fun i -> new_row.(i + 1) <- row.(i));
      new_row
    in
    Array.map ~f:add_row_col universe
  in
  let universe_expanded_columns =
    0 --^ Array.length universe.(0)
    |> Seq.to_rev_list
    |> List.fold_left
         ~f:(fun current_universe col ->
           if empty_col universe col then add_universe_col current_universe col
           else current_universe)
         ~init:universe
  in
  Array.flat_map
    ~f:(fun row -> if empty_row row then [| row; row |] else [| row |])
    universe_expanded_columns

let%test "universe expansion" =
  let contracted = parse sample in
  let expanded = expand contracted in
  let expected = parse expanded_universe in
  Test.(run bool (equal_universe expanded expected) ~expect:true)

let list_planets universe =
  Array.foldi
    ~f:(fun acc row_index ->
      Array.foldi
        ~f:(fun acc col_index c ->
          if Char.equal '#' c then (row_index, col_index) :: acc else acc)
        ~init:acc)
    ~init:[] universe

let%test "correct number of planets" =
  let planets = list_planets (parse sample |> expand) in
  Test.(run int (List.length planets) ~expect:9)

let%test "correct coordinates for planets" =
  let planets = list_planets (parse sample |> expand) in
  let has_first_planets =
    List.mem ~eq:(fun (a, b) (c, d) -> a = c && b = d) (0, 4) planets
  in
  Test.(run bool has_first_planets ~expect:true)

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

module Part_1 = struct
  let solve input =
    let universe = parse input in
    let universe = expand universe in
    let planets = list_planets universe in
    let double_distances =
      List.flat_map
        ~f:(fun p1 -> List.map ~f:(fun p2 -> manhattan_distance p1 p2) planets)
        planets
    in
    Util.sum double_distances / 2

  let%test "sample data" = Test.(run int (solve sample) ~expect:374)
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
