open ContainersLabels
open Seq.Infix

let sample =
  {|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
|}

type rock = Round | Square | Empty [@@deriving eq, show { with_path = false }]

let parse input =
  let parse_rock = function
    | 'O' -> Round
    | '#' -> Square
    | '.' -> Empty
    | _ -> failwith "parse_rock"
  in
  let parse_line line = String.to_array line |> Array.map ~f:parse_rock in
  String.lines_seq input |> Seq.map parse_line |> Seq.to_array

let%test "parse input" =
  Test.(run int (parse sample |> Array.length) ~expect:10)

let transpose grid =
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  let transpose = Array.init width ~f:(fun _ -> Array.make height Empty) in
  0 --^ height
  |> Seq.iter (fun i ->
         0 --^ width |> Seq.iter (fun j -> transpose.(j).(i) <- grid.(i).(j)));
  transpose

let migrate_north grid =
  let rec sort_row i j row =
    if i < 0 then ()
    else
      match (row.(i), row.(j)) with
      (* Move only the fix index looking for not round. *)
      | Round, Round -> sort_row (i - 1) j row
      (* Nothing gets past a Square to bring j forward. *)
      | Square, Round -> sort_row (i - 1) i row
      | Empty, Round ->
          row.(i) <- Round;
          row.(j) <- Empty;
          sort_row (i - 1) (j - 1) row
      | _ -> sort_row (i - 1) (j - 1) row
  in
  let sort platform =
    let i = Array.length platform.(0) - 2 in
    let j = Array.length platform.(0) - 1 in
    Array.iter ~f:(sort_row i j) platform
  in

  let t = transpose grid in
  sort t;
  transpose t

module Part_1 = struct
  let solve input =
    let platform = parse input in
    let platform = migrate_north platform in
    let len = Array.length platform in
    let count_rocks row =
      Array.filter ~f:(equal_rock Round) row |> Array.length
    in
    Array.foldi
      ~f:(fun acc index row -> acc + ((len - index) * count_rocks row))
      ~init:0 platform

  let%test "sample data" = Test.(run int (solve sample) ~expect:136)
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
