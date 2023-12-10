open ContainersLabels

let sample = {|
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
|} |> String.trim

let sample2 = {|
.....
.S-7.
.|.|.
.L-J.
.....
|} |> String.trim

let safe_get grid row col =
  if row < 0 || row >= Array.length grid then '.'
  else
    let line = grid.(row) in
    if col < 0 || col >= Array.length line then '.' else line.(col)

let find_s grid =
  let open Seq.Infix in
  let row_count = Array.length grid in
  let col_count = Array.length grid.(0) in
  0 -- (row_count - 1)
  |> Seq.flat_map (fun r -> 0 -- (col_count - 1) |> Seq.map (fun c -> (r, c)))
  |> Seq.filter (fun (r, c) -> Char.equal 'S' grid.(r).(c))
  |> Seq.head_exn

let guess_start grid (start_row, start_col) =
  let north = safe_get grid (start_row - 1) start_col in
  let south = safe_get grid (start_row + 1) start_col in
  let west = safe_get grid start_row (start_col - 1) in
  let east = safe_get grid start_row (start_col + 1) in
  let north_connected = List.mem ~eq:Char.equal north [ '|'; 'F'; '7' ] in
  let south_connected = List.mem ~eq:Char.equal south [ '|'; 'L'; 'J' ] in
  let east_connected = List.mem ~eq:Char.equal east [ '-'; 'J'; '7' ] in
  let west_connected = List.mem ~eq:Char.equal west [ '-'; 'F'; 'L' ] in
  match (north_connected, south_connected, east_connected, west_connected) with
  | true, true, false, false -> '|'
  | true, false, true, false -> 'L'
  | true, false, false, true -> 'J'
  | false, true, true, false -> 'F'
  | false, true, false, true -> '7'
  | false, false, true, true -> '-'
  | _ -> failwith "Invalid!"

type direction = North | South | East | West [@@deriving eq]

let find_loop grid (start_row, start_col) =
  let contains c cs = List.mem ~eq:Char.equal c cs in
  let rec next_point current_row current_col direction acc =
    let pipe = safe_get grid current_row current_col in
    if
      (not (List.is_empty acc))
      && current_row = start_row && current_col = start_col
    then acc
    else if
      (not (equal_direction direction North)) && contains pipe [ '|'; 'J'; 'L' ]
    then
      next_point (current_row - 1) current_col South
        ((current_row, current_col) :: acc)
    else if
      (not (equal_direction direction East)) && contains pipe [ '-'; 'L'; 'F' ]
    then
      next_point current_row (current_col + 1) West
        ((current_row, current_col) :: acc)
    else if
      (not (equal_direction direction South)) && contains pipe [ '|'; '7'; 'F' ]
    then
      next_point (current_row + 1) current_col North
        ((current_row, current_col) :: acc)
    else if
      (not (equal_direction direction West)) && contains pipe [ '-'; 'J'; '7' ]
    then
      next_point current_row (current_col - 1) East
        ((current_row, current_col) :: acc)
    else failwith "Invalid"
  in
  next_point start_row start_col West []

let parse input =
  let rows = String.lines input |> Array.of_list in
  Array.map ~f:(fun row -> String.to_seq row |> Seq.to_array) rows

module Part_1 = struct
  let solve input =
    let grid = parse input in
    let start = find_s grid in
    let s_pipe = guess_start grid start in
    grid.(fst start).(snd start) <- s_pipe;
    let loop = find_loop grid start in
    List.length loop / 2

  let%test "sample data" = Test.(run int (solve sample) ~expect:8)
  let%test "sample 2 data" = Test.(run int (solve sample2) ~expect:4)
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
