(* Hello, welcome to Advent of Code and OCaml! *)

open ContainersLabels

let sample = {|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|} |> String.trim

let sample_2 =
  {|
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|}
  |> String.trim

module Part_1 = struct
  let is_number c = Char.(c >= '0' && c <= '9')

  let solve input =
    let lines = input |> String.lines in
    let lines_of_digits = List.map ~f:(String.filter ~f:is_number) lines in
    let first_and_last_digits =
      List.map
        ~f:(fun s ->
          String.sub s ~pos:0 ~len:1
          ^ String.sub s ~pos:(String.length s - 1) ~len:1)
        lines_of_digits
    in
    List.map ~f:int_of_string first_and_last_digits
    |> List.fold_left ~f:Int.add ~init:0

  (* According to the description the expected value should be 7 given the
     sample data. *)
  let%test "sample data" = Test.(run int (solve sample) ~expect:142)
end

module Part_2 = struct
  let digits_regexp =
    [
      "one";
      "two";
      "three";
      "four";
      "five";
      "six";
      "seven";
      "eight";
      "nine";
      "1";
      "2";
      "3";
      "4";
      "5";
      "6";
      "7";
      "8";
      "9";
    ]
    |> List.map ~f:Re.str |> Re.alt |> Re.compile

  (* Numbers can overlap, e.g. eighthree should be parsed as 83. So we need to parse the string one digit at a time,
     and only progress of 1 char when we have a match.*)
  let extract_digits input =
    let module Group = Re.Group in
    let len = String.length input in
    let rec aux pos acc =
      let open Option.Infix in
      let next_match = Re.exec_opt ~pos digits_regexp input in
      let has_match =
        Option.get_or ~default:false
        @@ Option.map (fun group -> Group.test group 0) next_match
      in
      if has_match then
        let next_match = Option.get_exn_or "not possible" next_match in
        let digit = Group.get next_match 0 in
        let pos = Group.start next_match 0 + 1 in
        if pos = len then digit :: acc else aux pos (digit :: acc)
      else acc
    in
    List.rev @@ aux 0 []

  let spelled_out_digits =
    [
      ("one", "1");
      ("two", "2");
      ("three", "3");
      ("four", "4");
      ("five", "5");
      ("six", "6");
      ("seven", "7");
      ("eight", "8");
      ("nine", "9");
    ]

  (* Replace the spelled out digits with the actual digit. *)
  let unspell input =
    let aux input (spelled_out, digit) =
      String.replace ~which:`All ~sub:spelled_out ~by:digit input
    in
    List.fold_left ~f:aux ~init:input spelled_out_digits

  let solve input =
    let lines = input |> String.lines in
    let digits_per_line = List.map ~f:extract_digits lines in
    let first_and_last_per_line =
      List.map ~f:(fun l -> (List.hd l, List.hd @@ List.rev l)) digits_per_line
    in
    let numbers =
      List.map
        ~f:(fun (first, last) -> unspell first ^ unspell last)
        first_and_last_per_line
    in
    List.map ~f:int_of_string numbers |> List.fold_left ~f:Int.add ~init:0

  let%test "sample data" = Test.(run int (solve sample_2) ~expect:281)
  let%test "tricky input" = Test.(run int (solve "eighthree") ~expect:83)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
