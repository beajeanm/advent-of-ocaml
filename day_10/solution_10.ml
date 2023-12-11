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
  0 --^ row_count
  |> Seq.flat_map (fun r -> 0 --^ col_count |> Seq.map (fun c -> (r, c)))
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
    let p = Point.make ~x:current_row ~y:current_col in
    if
      (not (List.is_empty acc))
      && current_row = start_row && current_col = start_col
    then acc
    else if
      (not (equal_direction direction North)) && contains pipe [ '|'; 'J'; 'L' ]
    then next_point (current_row - 1) current_col South (p :: acc)
    else if
      (not (equal_direction direction East)) && contains pipe [ '-'; 'L'; 'F' ]
    then next_point current_row (current_col + 1) West (p :: acc)
    else if
      (not (equal_direction direction South)) && contains pipe [ '|'; '7'; 'F' ]
    then next_point (current_row + 1) current_col North (p :: acc)
    else if
      (not (equal_direction direction West)) && contains pipe [ '-'; 'J'; '7' ]
    then next_point current_row (current_col - 1) East (p :: acc)
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

let sample3 =
  {|
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
  |}
  |> String.trim

let sample4 =
  {|
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
  |}
  |> String.trim

module Part_2 = struct
  open Seq.Infix

  let is_inside polygons dot =
    let len = Array.length polygons in
    let crosses =
      0 --^ len |> Seq.to_list
      |> List.flat_map ~f:(fun i ->
             let p1 = polygons.(i) in
             let p2 = polygons.((i + 1) mod len) in
             let lefter = min (Point.x p1) (Point.x p2) in
             let righter = max (Point.x p1) (Point.x p2) in
             if Point.y p1 <> Point.y p2 || Point.y p1 < Point.y dot then []
             else if lefter < Point.x dot && righter >= Point.x dot then
               [ (p1, p2) ]
             else [])
    in
    List.length crosses |> fun c -> c mod 2 = 1

  let solve input =
    let grid = parse input in
    let start = find_s grid in
    let s_pipe = guess_start grid start in
    grid.(fst start).(snd start) <- s_pipe;
    let loop = find_loop grid start in
    let polygon =
      List.filter
        ~f:(fun p ->
          Char.(
            grid.(Point.x p).(Point.y p) <> '|'
            && grid.(Point.x p).(Point.y p) <> '-'))
        loop
      |> Array.of_list
    in
    let is_loop p = List.mem ~eq:Point.equal p loop in
    let is_inside = is_inside polygon in
    let count = ref 0 in
    for x = 0 to Array.length grid - 1 do
      for y = 0 to Array.length grid.(0) - 1 do
        let dot = Point.make ~x ~y in
        if (not (is_loop dot)) && is_inside dot then incr count else ()
      done
    done;
    !count

  let%test "sample data 3" = Test.(run int (solve sample3) ~expect:4)
  let%test "sample data 4" = Test.(run int (solve sample4) ~expect:8)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
