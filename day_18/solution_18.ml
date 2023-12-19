open ContainersLabels
open Seq.Infix

let sample =
  {|
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
|}
  |> String.trim

type direction = Right | Left | Up | Down
type instruction = direction * int

let parser input =
  let parse_direction = function
    | "R" -> Right
    | "L" -> Left
    | "D" -> Down
    | "U" -> Up
    | _ -> failwith "parse_direction"
  in

  let parse_row row =
    let groups = String.split_on_char ~by:' ' row in
    (parse_direction (List.nth groups 0), int_of_string (List.nth groups 1))
  in
  String.lines input |> List.map ~f:parse_row

let process_instruction point = function
  | Right, d -> Fun.iterate d Point.right point
  | Left, d -> Fun.iterate d Point.left point
  | Up, d -> Fun.iterate d Point.up point
  | Down, d -> Fun.iterate d Point.down point

let to_polygon instructions =
  let open Point in
  let start = make ~x:0 ~y:0 in
  let _, polygon =
    List.fold_left ~init:(start, [ start ])
      ~f:(fun (prev, poly) instruction ->
        let new_point = process_instruction prev instruction in
        (new_point, new_point :: poly))
      instructions
  in
  polygon |> Array.of_list

let shoelace polygon =
  let len = Array.length polygon in
  let res =
    0 --^ len
    |> Seq.fold_left
         (fun acc i ->
           let p1 = polygon.(i) in
           let p2 = polygon.((i + 1) mod len) in
           acc + Point.((p1.x * p2.y) - (p1.y * p2.x)))
         0
  in
  abs res / 2

let perimeter instructions = List.map ~f:snd instructions |> Util.sum

let surface instructions =
  let polygon = to_polygon instructions in
  let perimeter = perimeter instructions in
  let inside_area = shoelace polygon in
  inside_area + (perimeter / 2) + 1

module Part_1 = struct
  let solve input =
    let instructions = parser input in
    surface instructions

  let%test "sample data" = Test.(run int (solve sample) ~expect:62)
end

module Part_2 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:952408144115)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () = (* Run.solve_int (module Part_2); *) ()
