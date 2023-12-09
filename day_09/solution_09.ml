open ContainersLabels
open Seq.Infix

let sample = {|
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
|} |> String.trim

let parse input =
  let parse_line line =
    String.split_on_char ~by:' ' line
    |> List.map ~f:int_of_string |> Array.of_list
  in
  String.lines input |> List.map ~f:parse_line

let gen_next_row row line =
  let length = Array.length line in
  let next_line = Array.init length ~f:(Fun.const 0) in
  row -- (length - 1)
  |> Seq.iter (fun i -> next_line.(i) <- line.(i) - line.(i - 1));
  next_line

let gen_all line =
  let rec aux prev index acc =
    if Array.for_all ~f:(fun a -> a = 0) prev then acc
    else
      let next = gen_next_row index prev in
      aux next (index + 1) (next :: acc)
  in
  aux line 1 [ line ] |> List.rev

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    let gens = List.map ~f:gen_all parsed in
    List.flat_map
      ~f:(fun l -> List.map ~f:(fun a -> a.(Array.length a - 1)) l)
      gens
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:114)
end

module Part_2 = struct
  let solve input =
    let parsed = parse input in
    let gens = List.map ~f:gen_all parsed in
    let extrapolation =
      List.foldi ~init:0 ~f:(fun acc index arr ->
          acc + (arr.(index) * if index mod 2 = 0 then 1 else -1))
    in
    List.map ~f:extrapolation gens |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:2)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
