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

let find_vertical_reflexion grid =
  let is_reflexion row left_col =
    let bound = min (Array.length row - left_col - 2) left_col in
    let all_match =
      0 -- bound
      |> Seq.for_all (fun offset ->
             Char.equal row.(left_col - offset) row.(left_col + 1 + offset))
    in
    all_match
  in
  let first_row = grid.(0) in
  (* Check all indices of the first row to find a reflexion *)
  let first_row_reflexion =
    0 --^ (Array.length first_row - 1) |> Seq.filter (is_reflexion first_row)
  in
  (* Only keep the reflexion that is valid across all rows. *)
  Seq.filter
    (fun right_col ->
      Array.for_all ~f:(fun row -> is_reflexion row right_col) grid)
    first_row_reflexion
  (* We assume there is only one. *)
  |> Seq.head
  (*Arrays are 0 indexed. *)
  |> Option.map (Int.add 1)
  |> Option.value ~default:0

let find_horizontal_reflexion grid =
  let is_reflexion col_index up_row =
    let bound = min (Array.length grid - up_row - 2) up_row in
    let all_match =
      0 -- bound
      |> Seq.for_all (fun offset ->
             Char.equal
               grid.(up_row - offset).(col_index)
               grid.(up_row + 1 + offset).(col_index))
    in
    all_match
  in
  (* Check all indices of the first row to find a reflexion *)
  let first_col_reflexion =
    0 --^ (Array.length grid - 1) |> Seq.filter (is_reflexion 0)
  in
  (* Only keep the reflexion that is valid across all rows. *)
  Seq.filter
    (fun up_row ->
      0 --^ Array.length grid.(0)
      |> Seq.for_all (fun col -> is_reflexion col up_row))
    first_col_reflexion
  (* We assume there is only one. *)
  |> Seq.head (*Arrays are 0 indexed. *)
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
  Test.(run int (find_vertical_reflexion (to_grid pattern_1)) ~expect:5)

let%test "test vertical reflexion" =
  Test.(run int (find_horizontal_reflexion (to_grid pattern_2)) ~expect:400)

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    let horizontals = List.map ~f:find_horizontal_reflexion parsed in
    let verticals = List.map ~f:find_vertical_reflexion parsed in
    Util.sum horizontals + Util.sum verticals

  let%test "sample data" = Test.(run int (solve sample) ~expect:405)
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
