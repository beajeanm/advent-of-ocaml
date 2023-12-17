open ContainersLabels

let sample =
  {|
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|}
  |> String.trim

let sample_2 =
  {|
111111111111
999999999991
999999999991
999999999991
999999999991|}
  |> String.trim

(* A node contains the position in the grid, but also the direction to reach it and how long
   we've been on that path. This means we can visit a cell in the grid more than once with
   have multiple way to reach it. Since we are using dijsktra for path finding, we will do
   it only if the second path is cheaper than the first. *)
module Node = struct
  type direction = Up | Down | Left | Right
  [@@deriving eq, show { with_path = false }]

  let hash_direction = function Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3

  let opposite = function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

  type t = { point : Point.t; pred : direction option; straight_distance : int }
  [@@deriving eq, show { with_path = false }]

  let hash e =
    (CCHash.triple Point.hash (CCHash.opt hash_direction) CCHash.int)
      (e.point, e.pred, e.straight_distance)
end

module Graph = CCGraph

let parser input =
  String.lines input
  |> List.map ~f:(fun row ->
         String.to_array row
         |> Array.map ~f:(fun c -> Char.code c - Char.code '0'))
  |> Array.of_list

let iter_next_nodes next_node node yield =
  let open Node in
  [
    next_node node Left @@ Point.left node.point;
    next_node node Right @@ Point.right node.point;
    next_node node Up @@ Point.up node.point;
    next_node node Down @@ Point.down node.point;
  ]
  |> List.keep_some |> List.iter ~f:yield

let is_in_range grid p =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  Point.(p.x >= 0 && p.x < rows && p.y >= 0 && p.y < cols)

let solve0_ start_node last_point grid graph =
  let node_table = Graph.mk_table ~eq:Node.equal ~hash:Node.hash 10000 in
  let iterator () =
    Graph.Traverse.dijkstra ~dist:Fun.id ~tbl:node_table ~graph
      (Iter.return start_node)
  in
  Iter.filter
    (fun (node, _, _) -> Point.equal last_point node.Node.point)
    (iterator ())
  |> Iter.head_exn
  |> fun (_, cost, _) -> cost

let solve0 grid =
  let start_node =
    Node.{ point = Point.make ~x:0 ~y:0; pred = None; straight_distance = 0 }
  in
  let last_point =
    Point.make ~x:(Array.length grid - 1) ~y:(Array.length grid.(0) - 1)
  in
  solve0_ start_node last_point grid

module Part_1 = struct
  let maybe_next_node grid node direction point =
    let open Node in
    let straight =
      Option.map (equal_direction direction) node.pred
      |> Option.value ~default:false
    in
    let straight_distance =
      if straight then node.straight_distance + 1 else 0
    in
    let is_reverse =
      Option.map opposite node.pred
      |> Option.map (equal_direction direction)
      |> Option.value ~default:false
    in
    (* Only yield if the vertice is valid:
       - we are still on the grid
       - we can't turn back
       - we can't move more than 3 steps in the same direction *)
    if is_in_range grid point && straight_distance < 3 && not is_reverse then
      Some
        ( Point.data grid point,
          { point; pred = Some direction; straight_distance } )
    else None

  let solve input =
    let grid = parser input in
    let graph = Graph.make @@ iter_next_nodes @@ maybe_next_node grid in
    solve0 grid graph

  let%test "sample data" = Test.(run int (solve sample) ~expect:102)
end

module Part_2 = struct
  let maybe_next_node grid node direction point =
    let open Node in
    (* We are moving in a straight line *)
    let straight =
      Option.map (equal_direction direction) node.pred
      |> Option.value ~default:true
    in

    let straight_distance =
      if straight then node.straight_distance + 1 else 1
    in
    let is_too_close =
      let remaining =
        match direction with
        | Up -> point.Point.x
        | Down -> Array.length grid - point.Point.x - 1
        | Left -> point.Point.y
        | Right -> Array.length grid.(0) - point.Point.y - 1
      in
      remaining + straight_distance < 4
    in
    let is_reverse =
      Option.map opposite node.pred
      |> Option.map (equal_direction direction)
      |> Option.value ~default:false
    in
    if (not straight) && node.straight_distance < 4 then None
    else if straight_distance > 10 then None
    else if is_reverse then None
    else if not (is_in_range grid point) then None
    else if is_too_close then None
    else
      Some
        ( Point.data grid point,
          { point; pred = Some direction; straight_distance } )

  let solve input =
    let grid = parser input in
    let graph = Graph.make @@ iter_next_nodes @@ maybe_next_node grid in
    solve0 grid graph

  let%test "sample data" = Test.(run int (solve sample) ~expect:94)
  let%test "sample data" = Test.(run int (solve sample_2) ~expect:71)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
