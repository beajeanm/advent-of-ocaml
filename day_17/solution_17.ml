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

  type t = { point : Point.t; pred : direction; straight_distance : int }
  [@@deriving eq, show { with_path = false }]

  let hash e =
    (CCHash.triple Point.hash hash_direction CCHash.int)
      (e.point, e.pred, e.straight_distance)
end

module Graph = CCGraph

let parser input =
  String.lines input
  |> List.map ~f:(fun row ->
         String.to_array row
         |> Array.map ~f:(fun c -> Char.code c - Char.code '0'))
  |> Array.of_list

let iter_next_nodes grid validate_node node yield =
  let open Node in
  let next_node node direction point =
    let open Node in
    (* We are moving in a straight line *)
    let straight = equal_direction direction node.pred in

    let straight_distance =
      if straight then node.straight_distance + 1 else 1
    in
    Some
      (Point.safe_get grid point, { point; pred = direction; straight_distance })
    |> Option.filter (fun (_, new_node) -> validate_node grid node new_node)
    |> Option.map (fun (cost_opt, node) ->
           (Option.get_exn_or "missing cost" cost_opt, node))
  in
  [
    next_node node Left @@ Point.left node.point;
    next_node node Right @@ Point.right node.point;
    next_node node Up @@ Point.up node.point;
    next_node node Down @@ Point.down node.point;
  ]
  |> List.keep_some |> List.iter ~f:yield

let is_in_range grid p = Point.safe_get grid p |> Option.is_some

let dijkstra_cost grid graph =
  (* Since the way the direction and stint are part of the node,
     We have 2 starting position from (0,0) based on the direction we intend to take.*)
  let start_nodes =
    [
      Node.{ point = Point.make ~x:0 ~y:0; pred = Right; straight_distance = 1 };
      Node.{ point = Point.make ~x:0 ~y:0; pred = Down; straight_distance = 1 };
    ]
  in
  let node_table = Graph.mk_table ~eq:Node.equal ~hash:Node.hash 10000 in
  let paths () =
    Graph.Traverse.dijkstra ~dist:Fun.id ~tbl:node_table ~graph
      (Iter.of_list start_nodes)
  in
  let is_target =
    Point.(
      equal @@ make ~x:(Array.length grid - 1) ~y:(Array.length grid.(0) - 1))
  in
  paths ()
  |> Iter.filter (fun (node, _, _) -> is_target node.Node.point)
  |> Iter.head_exn
  |> fun (_, cost, _) -> cost

module Part_1 = struct
  let validate_node grid node new_node =
    let open Node in
    let is_reverse = equal_direction new_node.pred (opposite node.pred) in
    let is_in_range = is_in_range grid new_node.point in
    let can_continue = new_node.straight_distance <= 3 in
    is_in_range && can_continue && not is_reverse

  let solve input =
    let grid = parser input in
    let graph = Graph.make @@ iter_next_nodes grid validate_node in
    dijkstra_cost grid graph

  let%test "sample data" = Test.(run int (solve sample) ~expect:102)
end

module Part_2 = struct
  let validate_node grid old_node new_node =
    let open Node in
    let is_too_close =
      let remaining =
        match new_node.pred with
        | Up -> new_node.point.x
        | Down -> Array.length grid - new_node.point.x - 1
        | Left -> new_node.point.y
        | Right -> Array.length grid.(0) - new_node.point.y - 1
      in
      remaining + new_node.straight_distance < 4
    in

    let straight = equal_direction new_node.pred old_node.pred in
    (*We can't turn back*)
    let is_reverse = equal_direction new_node.pred (opposite old_node.pred) in
    (*We can only turn after going straight for 4 steps*)
    let forbidden_turn = (not straight) && old_node.straight_distance < 4 in
    (*We can't go straight for more than 10 steps*)
    let will_wobble = new_node.straight_distance > 10 in
    (*We can't get out of the grid*)
    let is_in_range = is_in_range grid new_node.point in

    (not forbidden_turn) && (not will_wobble) && (not is_reverse) && is_in_range
    && not is_too_close

  let solve input =
    let grid = parser input in
    let graph = Graph.make @@ iter_next_nodes grid validate_node in
    dijkstra_cost grid graph

  let%test "sample data" = Test.(run int (solve sample) ~expect:94)
  let%test "sample data" = Test.(run int (solve sample_2) ~expect:71)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
