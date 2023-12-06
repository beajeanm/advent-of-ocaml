open ContainersLabels
open Iter.Infix

let sample = {|
Time:      7  15   30
Distance:  9  40  200
|} |> String.trim

let margin_of_error time distance =
  0 -- time
  |> Iter.map (fun pressed -> pressed * (time - pressed))
  |> Iter.filter (fun result -> result > distance)
  |> Iter.length

module Part_1 = struct
  let parser =
    let open Angstrom in
    let open Util.Parser in
    let+ times =
      string "Time:" *> whitespaces *> sep_by whitespaces integer <* end_of_line
    and+ distances =
      string "Distance:" *> whitespaces *> sep_by whitespaces integer
    in
    (times, distances)

  let solve input =
    let times, distances =
      Angstrom.parse_string ~consume:Prefix parser input |> Result.get_exn
    in
    let margin_of_errors = List.map2 ~f:margin_of_error times distances in
    Util.prod margin_of_errors

  let%test "sample data" = Test.(run int (solve sample) ~expect:288)
end

module Part_2 = struct
  let parser =
    let open Angstrom in
    let open Util.Parser in
    let integer = take_while1 is_int in
    let+ times =
      string "Time:" *> whitespaces *> sep_by whitespaces integer <* end_of_line
    and+ distances =
      string "Distance:" *> whitespaces *> sep_by whitespaces integer
    in
    let to_int s = String.concat ~sep:"" s |> int_of_string in
    (to_int times, to_int distances)

  let solve input =
    let time, distance =
      Angstrom.parse_string ~consume:Prefix parser input |> Result.get_exn
    in
    margin_of_error time distance

  let%test "sample data" = Test.(run int (solve sample) ~expect:71503)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
