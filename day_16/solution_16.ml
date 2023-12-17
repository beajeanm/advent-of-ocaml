open ContainersLabels

let sample =
  {|
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
|}
  |> String.trim

type direction = Right | Down | Left | Up
[@@deriving eq, show { with_path = false }, ord]

type space = Slash | Backslash | Empty | Horizontal | Vertical
[@@deriving eq, show { with_path = false }, ord]

let compare_tuple p1 p2 =
  let cmp2 = Int.compare p1.Point.x p2.Point.x in
  if cmp2 = 0 then Int.compare p1.Point.y p2.Point.y else cmp2

module State = struct
  type t = { coordinates : Point.t; direction : direction }
  [@@deriving eq, show { with_path = false }]

  let compare s1 s2 =
    let cmp = compare_direction s1.direction s2.direction in
    if cmp = 0 then compare_tuple s1.coordinates s2.coordinates else cmp
end

open State

let next_state grid state =
  let update_direction direction mirror =
    match (direction, mirror) with
    | _, Empty
    | Left, Horizontal
    | Right, Horizontal
    | Up, Vertical
    | Down, Vertical ->
        [ direction ]
    | Right, Backslash | Left, Slash -> [ Down ]
    | Right, Slash | Left, Backslash -> [ Up ]
    | Down, Backslash | Up, Slash -> [ Right ]
    | Down, Slash | Up, Backslash -> [ Left ]
    | Up, Horizontal | Down, Horizontal -> [ Right; Left ]
    | Left, Vertical | Right, Vertical -> [ Up; Down ]
  in
  let update_coordinates { coordinates; direction } =
    let coordinates =
      match direction with
      | Up -> Point.up coordinates
      | Down -> Point.down coordinates
      | Left -> Point.left coordinates
      | Right -> Point.right coordinates
    in
    let row = coordinates.Point.x in
    let col = coordinates.Point.y in
    if
      row < 0
      || row >= Array.length grid
      || col < 0
      || col >= Array.length grid.(0)
    then None
    else Some coordinates
  in
  let direction = state.direction in
  let space = Point.data grid state.coordinates in
  let new_states =
    update_direction direction space
    |> List.map ~f:(fun direction -> { state with direction })
    |> List.map ~f:(fun state ->
           update_coordinates state
           |> Option.map (fun coordinates -> { state with coordinates }))
    |> List.keep_some
  in
  new_states

module StateSet = Set.Make (State)

let process_all_states grid initial_state =
  let queue = CCDeque.create () in
  let memo = ref StateSet.empty in
  let add_queue state =
    Ref.update (StateSet.add state) memo;
    CCDeque.push_front queue state
  in
  add_queue initial_state;
  while not (CCDeque.is_empty queue) do
    let state = CCDeque.take_front queue in
    next_state grid state
    |> List.filter ~f:(fun state -> not (StateSet.mem state !memo))
    |> List.iter ~f:add_queue
  done;
  StateSet.to_seq !memo
  |> Seq.map (fun state -> state.coordinates)
  |> Seq.sort ~cmp:compare_tuple
  |> Seq.uniq Point.equal |> Seq.to_list |> List.length

let parse input =
  let parse_space = function
    | '.' -> Empty
    | '/' -> Slash
    | '\\' -> Backslash
    | '|' -> Vertical
    | '-' -> Horizontal
    | _ -> failwith "parse_space"
  in
  let parse_row row = String.to_array row |> Array.map ~f:parse_space in
  String.lines input |> List.map ~f:parse_row |> Array.of_list

module Part_1 = struct
  let solve input =
    let initial_state =
      { coordinates = Point.make ~x:0 ~y:0; direction = Right }
    in
    let grid = parse input in
    process_all_states grid initial_state

  let%test "sample data" = Test.(run int (solve sample) ~expect:46)
end

module Part_2 = struct
  open Seq.Infix

  let solve input =
    let grid = parse input in
    let rows = Array.length grid in
    let cols = Array.length grid.(0) in
    let row_coordinates =
      0 --^ rows
      |> Seq.flat_map (fun row ->
             Seq.of_list
               [
                 { coordinates = Point.make ~x:row ~y:0; direction = Right };
                 {
                   coordinates = Point.make ~x:row ~y:(cols - 1);
                   direction = Left;
                 };
               ])
    in
    let col_coordinates =
      0 --^ cols
      |> Seq.flat_map (fun col ->
             Seq.of_list
               [
                 { coordinates = Point.make ~x:0 ~y:col; direction = Down };
                 {
                   coordinates = Point.make ~x:(rows - 1) ~y:col;
                   direction = Up;
                 };
               ])
    in
    let all_data =
      Seq.append row_coordinates col_coordinates
      |> Seq.map (fun state -> process_all_states grid state)
      |> Seq.to_list
      |> List.sort ~cmp:(fun a b -> Int.compare b a)
    in
    List.hd all_data

  let%test "sample data" = Test.(run int (solve sample) ~expect:51)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
