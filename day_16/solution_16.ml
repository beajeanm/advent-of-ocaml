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

let equal_tuple (a1, b1) (a2, b2) = Int.equal a1 a2 && Int.equal b1 b2

let compare_tuple (a1, b1) (a2, b2) =
  let cmp2 = Int.compare a1 a2 in
  if cmp2 = 0 then Int.compare b1 b2 else cmp2

module State = struct
  type t = { coordinates : int * int; direction : direction }
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
  let update_coordinates { coordinates = row, col; direction } =
    let row, col =
      match direction with
      | Up -> (row - 1, col)
      | Down -> (row + 1, col)
      | Left -> (row, col - 1)
      | Right -> (row, col + 1)
    in
    if
      row < 0
      || row >= Array.length grid
      || col < 0
      || col >= Array.length grid.(0)
    then None
    else Some (row, col)
  in
  let row, col = state.coordinates in
  let direction = state.direction in
  let space = grid.(row).(col) in
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
  CCDeque.push_back queue initial_state;
  Ref.update (StateSet.add initial_state) memo;
  while not (CCDeque.is_empty queue) do
    let state = CCDeque.take_front queue in
    next_state grid state
    |> List.filter ~f:(fun state -> not (StateSet.mem state !memo))
    |> List.iter ~f:add_queue
  done;
  StateSet.to_seq !memo
  |> Seq.map (fun state -> state.coordinates)
  |> Seq.sort ~cmp:compare_tuple
  |> Seq.uniq equal_tuple |> Seq.to_list |> List.length

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
    let initial_state = { coordinates = (0, 0); direction = Right } in
    let grid = parse input in
    process_all_states grid initial_state

  let%test "sample data" = Test.(run int (solve sample) ~expect:46)
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
