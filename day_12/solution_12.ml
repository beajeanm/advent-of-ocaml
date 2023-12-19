open ContainersLabels

let sample =
  {|
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|}
  |> String.trim

let count recurse (row, counts) =
  let contains = List.mem ~eq:Char.equal in
  let maybe_operational c = Char.(equal '.' c || equal '?' c) in
  let maybe_broken c = Char.(equal '#' c || equal '?' c) in
  if List.is_empty row then if List.is_empty counts then 1 else 0
  else if List.is_empty counts then if contains '#' row then 0 else 1
  else
    let first_char = List.hd row in
    let current_count = List.hd counts in
    let good_count =
      if maybe_operational first_char then recurse (List.tl row, counts) else 0
    in
    let broken_count =
      if maybe_broken first_char then
        if
          (* Enough pipes to match the current group? *)
          List.length row > current_count
          (* An operational pipe in the next group? *)
          && (not (List.take current_count row |> contains '.'))
          (* After the current group we need an operational pipe. *)
          && maybe_operational (List.nth row current_count)
        then recurse (List.drop (current_count + 1) row, List.tl counts)
        else 0
      else 0
    in
    good_count + broken_count

module Key = struct
  type t = char list * int list [@@deriving eq]

  let hash = CCHash.(pair (list char) (list int))
end

module KeyMap = Hashtbl.Make (Key)

let count' row counts =
  (* Add an operational spring at the end to simplify some edge cases. *)
  let row = List.concat [ row; [ '.' ] ] in
  let cache = CCCache.unbounded ~eq:Key.equal ~hash:Key.hash 10000 in
  CCCache.with_cache_rec cache count (row, counts)

let%test "Count broken groups" =
  Test.(run int (count' ("#.#.###." |> String.to_list) [ 1; 1; 3 ]) ~expect:1)

let%test "Count broken groups 2" =
  Test.(
    run int
      (count' ("#....######..#####." |> String.to_list) [ 1; 6; 5 ])
      ~expect:1)

let%test "counting arrangements 1" =
  Test.(run int (count' ("???.###." |> String.to_list) [ 1; 1; 3 ]) ~expect:1)

let%test "counting arrangements 3" =
  Test.(
    run int (count' ("?###????????." |> String.to_list) [ 3; 2; 1 ]) ~expect:10)

let parse input =
  let parse_row row =
    let groups = String.split_on_char ~by:' ' row in
    let num_groups =
      String.split_on_char ~by:',' (List.get_at_idx_exn 1 groups)
    in
    (List.hd groups, List.map ~f:int_of_string num_groups)
  in
  String.lines input |> List.map ~f:parse_row

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    List.map ~f:(fun (row, counts) -> count' (String.to_list row) counts) parsed
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:21)
end

module Part_2 = struct
  let solve input =
    let unfold (row, counts) =
      ( String.concat ~sep:"?" [ row; row; row; row; row ],
        List.concat [ counts; counts; counts; counts; counts ] )
    in
    let parsed = parse input |> List.map ~f:unfold in
    List.map ~f:(fun (row, counts) -> count' (String.to_list row) counts) parsed
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:525152)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
