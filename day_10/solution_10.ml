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

let safe_get grid point =
  let row = point.Point.x in
  let col = point.Point.y in
  if row < 0 || row >= Array.length grid then '.'
  else
    let line = grid.(row) in
    if col < 0 || col >= Array.length line then '.' else line.(col)

let find_s grid =
  let open Seq.Infix in
  let row_count = Array.length grid in
  let col_count = Array.length grid.(0) in
  0 --^ row_count
  |> Seq.flat_map (fun x ->
         0 --^ col_count |> Seq.map (fun y -> Point.make ~x ~y))
  |> Seq.filter (fun p -> Char.equal 'S' grid.(p.Point.x).(p.Point.y))
  |> Seq.head_exn

type direction = North | South | East | West [@@deriving eq, show]

let connection = function
  | '|' -> [ North; South ]
  | 'J' -> [ North; West ]
  | 'L' -> [ North; East ]
  | '-' -> [ East; West ]
  | 'F' -> [ East; South ]
  | '7' -> [ West; South ]
  | _ -> []

let guess_start grid start =
  let north = safe_get grid (Point.up start) in
  let south = safe_get grid (Point.down start) in
  let west = safe_get grid (Point.left start) in
  let east = safe_get grid (Point.right start) in
  let north_connection =
    List.mem ~eq:equal_direction South (connection north)
  in
  let south_connection =
    List.mem ~eq:equal_direction North (connection south)
  in
  let east_connection = List.mem ~eq:equal_direction West (connection east) in
  let west_connection = List.mem ~eq:equal_direction East (connection west) in
  match
    (north_connection, south_connection, east_connection, west_connection)
  with
  | true, true, false, false -> '|'
  | true, false, true, false -> 'L'
  | true, false, false, true -> 'J'
  | false, true, true, false -> 'F'
  | false, true, false, true -> '7'
  | false, false, true, true -> '-'
  | _ -> failwith "Invalid!"

let opposite = function
  | North -> South
  | South -> North
  | East -> West
  | West -> East

let find_loop grid start =
  let rec iteration current from acc =
    let pipe = safe_get grid current in
    let next_direction =
      connection pipe
      |> List.filter ~f:(Fun.negate @@ equal_direction from)
      |> List.hd
    in
    let next_point =
      match next_direction with
      | South -> Point.down current
      | North -> Point.up current
      | West -> Point.left current
      | East -> Point.right current
    in
    if (not (List.is_empty acc)) && Point.equal current start then acc
    else iteration next_point (opposite next_direction) (current :: acc)
  in

  iteration start West []

let parse input =
  let rows = String.lines input |> Array.of_list in
  Array.map ~f:(fun row -> String.to_seq row |> Seq.to_array) rows

module Part_1 = struct
  let solve input =
    let grid = parse input in
    let start = find_s grid in
    let s_pipe = guess_start grid start in
    grid.(start.Point.x).(start.Point.y) <- s_pipe;
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
             let lefter = min p1.Point.x p2.Point.x in
             let righter = max p1.Point.x p2.Point.x in
             if p1.Point.y <> p2.Point.y || p1.Point.y < dot.Point.y then []
             else if lefter < dot.Point.x && righter >= dot.Point.x then
               [ (p1, p2) ]
             else [])
    in
    List.length crosses |> fun c -> c mod 2 = 1

  let solve input =
    let grid = parse input in
    let start = find_s grid in
    let s_pipe = guess_start grid start in
    grid.(start.Point.x).(start.Point.y) <- s_pipe;
    let loop = find_loop grid start in
    let polygon =
      List.filter
        ~f:(fun p ->
          Char.(
            grid.(p.Point.x).(p.Point.y) <> '|'
            && grid.(p.Point.x).(p.Point.y) <> '-'))
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
