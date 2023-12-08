open ContainersLabels

let sample =
  {|
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
  |}
  |> String.trim

let sample2 =
  {|
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
  |} |> String.trim

let sample3 =
  {|
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
  |}
  |> String.trim

type direction = Right | Left | NoDirection

module StringMap = Map.Make (String)

module RingBuffer = CCRingBuffer.Make (struct
  type t = direction

  let dummy = NoDirection
end)

let parse input =
  let parse_direction = function
    | 'R' -> Right
    | 'L' -> Left
    | _ -> failwith "Unexpected direction"
  in
  let lines = String.lines input in
  let directions =
    List.hd lines |> String.to_seq |> Seq.map parse_direction |> Seq.to_array
    |> RingBuffer.of_array
  in
  let parse_links line =
    Scanf.sscanf line "%s = (%s@, %s@)" (fun a b c -> (a, (b, c)))
  in
  let links = List.drop 2 lines |> List.map ~f:parse_links in
  let mapping = StringMap.add_list StringMap.empty links in
  (directions, mapping)

module Part_1 = struct
  let go_to_zzz directions mapping =
    let rec aux count = function
      | "ZZZ" -> count
      | position -> (
          let left, right =
            StringMap.get position mapping
            |> Option.get_exn_or "Missing mapping"
          in
          let next_direction = RingBuffer.take_front_exn directions in
          RingBuffer.push_back directions next_direction;
          match next_direction with
          | Right -> aux (count + 1) right
          | Left -> aux (count + 1) left
          | _ -> failwith "Invalid direction")
    in
    aux 0 "AAA"

  let solve input =
    let directions, mapping = parse input in
    go_to_zzz directions mapping

  let%test "sample data" = Test.(run int (solve sample) ~expect:2)
  let%test "sample data" = Test.(run int (solve sample2) ~expect:6)
end

module Part_2 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample3) ~expect:6)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
