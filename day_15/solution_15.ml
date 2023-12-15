open ContainersLabels

let sample =
  {|
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
|} |> String.trim

let hash =
  let aux h c = (h + Char.code c) * 17 mod 256 in
  String.fold_left ~f:aux ~init:0

let parse = String.split_on_char ~by:','

module Part_1 = struct
  let solve input = parse input |> List.map ~f:hash |> Util.sum
  let%test "sample data" = Test.(run int (solve sample) ~expect:1320)
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
