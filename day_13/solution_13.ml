open ContainersLabels
open Seq.Infix

let sample =
  {|
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|}
  |> String.trim

let pattern_1 =
  {|
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
|}
  |> String.trim

let pattern_2 =
  {|
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|}
  |> String.trim

let other_sample =
  {|
...
..#
...

...
...
.#.

...
#..
...

.#.
...
...

...
.#.
...|}
  |> String.trim

let compare c1 c2 = if Char.equal c1 c2 then 0 else 1

let find_vertical_reflexion allowed_mismatches grid =
  let is_almost_reflexion allowed_mismatches row left_col =
    let bound = min (Array.length row - left_col - 2) left_col in
    let mismatches =
      0 -- bound
      |> Seq.map (fun offset ->
             compare row.(left_col - offset) row.(left_col + 1 + offset))
      |> Seq.fold_left Int.add 0
    in
    mismatches = allowed_mismatches
  in
  let is_reflexion = is_almost_reflexion 0 in
  let reflexion_for_row row_index =
    0 --^ (Array.length grid.(row_index) - 1)
    |> Seq.filter (is_almost_reflexion allowed_mismatches grid.(row_index))
  in
  let valid_for_other_rows row_index left_col =
    0 --^ Array.length grid
    |> Seq.filter (Fun.negate @@ Int.equal row_index)
    |> Seq.for_all (fun row -> is_reflexion grid.(row) left_col)
  in
  0 --^ Array.length grid
  |> Seq.flat_map (fun row ->
         reflexion_for_row row
         |> Seq.filter (fun col -> valid_for_other_rows row col))
  |> Seq.head
  |> Option.map (Int.add 1)
  |> Option.value ~default:0

let find_horizontal_reflexion allowed_mismatches grid =
  let is_almost_reflexion allowed_mismatches col_index up_row =
    let bound = min (Array.length grid - up_row - 2) up_row in
    let mismatches =
      0 -- bound
      |> Seq.map (fun offset ->
             compare
               grid.(up_row - offset).(col_index)
               grid.(up_row + 1 + offset).(col_index))
      |> Seq.fold_left Int.add 0
    in
    mismatches = allowed_mismatches
  in
  let is_reflexion = is_almost_reflexion 0 in
  let reflexions_for_col col_index =
    0 --^ (Array.length grid - 1)
    |> Seq.filter (is_almost_reflexion allowed_mismatches col_index)
  in
  let valid_for_other_cols col_index up_row =
    0 --^ Array.length grid.(0)
    |> Seq.filter (Fun.negate @@ Int.equal col_index)
    |> Seq.for_all (fun col -> is_reflexion col up_row)
  in
  0 --^ Array.length grid.(0)
  |> Seq.flat_map (fun col ->
         reflexions_for_col col
         |> Seq.filter (fun row -> valid_for_other_cols col row))
  |> Seq.head
  |> Option.map (Int.add 1)
  |> Option.map (Int.mul 100)
  |> Option.value ~default:0

let to_grid input =
  String.lines_seq input
  |> Seq.map (fun row -> String.to_array row)
  |> Seq.to_array

let parse input =
  let parser =
    let open Angstrom in
    let line = take_till (Char.equal '\n') |> map ~f:String.to_array in
    sep_by end_of_line line
  in
  let parsed =
    Angstrom.parse_string ~consume:All parser input |> Result.get_exn
  in
  List.group_succ
    ~eq:(fun a b -> Bool.equal (Array.length a = 0) (Array.length b = 0))
    parsed
  |> List.filter ~f:(fun l -> List.length l > 1)
  |> List.map ~f:(fun l -> Array.of_list l)

let%test "test vertical reflexion" =
  Test.(run int (find_vertical_reflexion 0 (to_grid pattern_1)) ~expect:5)

let%test "test vertical reflexion" =
  Test.(run int (find_horizontal_reflexion 0 (to_grid pattern_2)) ~expect:400)

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    let horizontals = List.map ~f:(find_horizontal_reflexion 0) parsed in
    let verticals = List.map ~f:(find_vertical_reflexion 0) parsed in
    Util.sum horizontals + Util.sum verticals

  let%test "sample data" = Test.(run int (solve sample) ~expect:405)
  let%test "sample data" = Test.(run int (solve other_sample) ~expect:303)
end

module Part_2 = struct
  let solve input =
    let parsed = parse input in
    let horizontals = List.map ~f:(find_horizontal_reflexion 1) parsed in
    let verticals = List.map ~f:(find_vertical_reflexion 1) parsed in
    Util.sum horizontals + Util.sum verticals

  let%test "sample data" = Test.(run int (solve sample) ~expect:400)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
