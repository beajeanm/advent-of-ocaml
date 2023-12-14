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

let show_rock = function Round -> "O" | Square -> "#" | Empty -> "."
let hash_rock = function Round -> 7 | Square -> 23 | Empty -> 59

module Platform = struct
  type t = rock Array.t Array.t [@@deriving eq]

  let show_platform platform =
    Array.map
      ~f:(fun row ->
        Array.map ~f:show_rock row |> Seq.of_array |> String.concat_seq ~sep:" ")
      platform
    |> Seq.of_array
    |> String.concat_seq ~sep:"\n"

  let hash (p : t) =
    Array.fold_left
      ~f:(fun hash ->
        Array.fold_left ~f:(fun hash rock -> hash + hash_rock rock) ~init:hash)
      ~init:0 p
end

let parse input =
  let parse_rock = function
    | 'O' -> Round
    | '#' -> Square
    | '.' -> Empty
    | _ -> failwith "parse_rock"
  in
  let parse_line line = String.to_array line |> Array.map ~f:parse_rock in
  String.lines_seq input |> Seq.map parse_line |> Seq.to_array

let transpose grid =
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  let transpose = Array.init width ~f:(fun _ -> Array.make height Empty) in
  0 --^ height
  |> Seq.iter (fun i ->
         0 --^ width |> Seq.iter (fun j -> transpose.(j).(i) <- grid.(i).(j)));
  transpose

let flip platform =
  let flip_row row =
    let len = Array.length row in
    0 --^ (len / 2)
    |> Seq.iter (fun i ->
           let tmp = row.(i) in
           row.(i) <- row.(len - 1 - i);
           row.(len - 1 - i) <- tmp)
  in
  Array.iter ~f:flip_row platform

let sort platform =
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
  let i = Array.length platform.(0) - 2 in
  let j = Array.length platform.(0) - 1 in
  Array.iter ~f:(sort_row i j) platform

let migrate_north platform =
  let t = transpose platform in
  sort t;
  transpose t

let migrate_west platform =
  sort platform;
  platform

let migrate_east platform =
  flip platform;
  sort platform;
  flip platform;
  platform

let migrate_south platform =
  let t = transpose platform in
  flip t;
  sort t;
  flip t;
  transpose t

let%test "parse input" =
  Test.(run int (parse sample |> Array.length) ~expect:10)

let load platform =
  let len = Array.length platform in
  let count_rocks row =
    Array.filter ~f:(equal_rock Round) row |> Array.length
  in
  Array.foldi
    ~f:(fun acc index row -> acc + ((len - index) * count_rocks row))
    ~init:0 platform

let spin platform =
  migrate_north platform |> migrate_west |> migrate_south |> migrate_east

module PlatformMap = Hashtbl.Make (Platform)

let spin' spins platform =
  (* Save us from infinite loops *)
  let max_iteration = 200 in
  let cache = PlatformMap.create max_iteration in
  let rec aux iteration platform =
    if iteration = max_iteration then failwith "Not cycle found"
    else
      let platform = spin platform in
      let prev = PlatformMap.find_opt cache platform in
      if Option.is_some prev then
        let prev = Option.get_exn_or "" prev in
        (prev, iteration - prev)
      else (
        PlatformMap.add cache platform iteration;
        aux (iteration + 1) platform)
  in
  PlatformMap.add cache platform 0;
  let start_cyle, cycle_len = aux 1 platform in
  0 --^ (((spins - start_cyle) mod cycle_len) + start_cyle)
  |> Seq.fold_left (fun p _ -> spin p) platform

module Part_1 = struct
  let solve input =
    let platform = parse input in
    let platform = migrate_north platform in
    load platform

  let%test "sample data" = Test.(run int (solve sample) ~expect:136)
end

module Part_2 = struct
  let solve input =
    let platform = parse input in
    let spins = 1000000000 in
    spin' spins platform |> load

  let%test "sample data" = Test.(run int (solve sample) ~expect:64)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
