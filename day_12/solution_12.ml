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

let count_contiguous row =
  let grouped = List.group_succ ~eq:Char.equal row in
  List.filter ~f:(fun cs -> Char.equal '#' (List.hd cs)) grouped
  |> List.map ~f:List.length

let%test "Count broken groups" =
  Test.(
    run (list int)
      (count_contiguous ("#.#.###" |> String.to_list))
      ~expect:[ 1; 1; 3 ])

let%test "Count broken groups 2" =
  Test.(
    run (list int)
      (count_contiguous ("#....######..#####." |> String.to_list))
      ~expect:[ 1; 6; 5 ])

let arrangements row =
  let rec aux prefix = function
    | [] -> [ List.rev prefix ]
    | '?' :: cs ->
        List.concat [ aux ('#' :: prefix) cs; aux ('.' :: prefix) cs ]
    | c :: cs -> aux (c :: prefix) cs
  in
  aux [] row

let matching_arrangements row counts =
  let eq = List.equal Int.equal in
  arrangements row
  |> List.filter ~f:(fun a -> eq counts (count_contiguous a))
  |> List.length

let%test "counting arrangements 1" =
  Test.(
    run int
      (matching_arrangements ("???.###" |> String.to_list) [ 1; 1; 3 ])
      ~expect:1)

let%test "counting arrangements 3" =
  Test.(
    run int
      (matching_arrangements ("?###????????" |> String.to_list) [ 3; 2; 1 ])
      ~expect:10)

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
    List.map
      ~f:(fun (row, counts) ->
        matching_arrangements (String.to_list row) counts)
      parsed
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:21)
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
