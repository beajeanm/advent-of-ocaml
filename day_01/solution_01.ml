(* Hello, welcome to Advent of Code and OCaml! *)

open ContainersLabels
(* open Containers *)

(* I prefer labeled arguments.

   - This is `fold_left` with labeled argumnets, ~f and ~init are labels.
     `List.fold_left ~f:( + ) ~init:0`
     `List.fold_left ~init:0 ~f:( + )`

   - Labeled arguments also have "punning", meaning that if `f` is defined
     `let f = ( + )`
     then we can omit the `:` part completly
     `List.fold_left ~init:0` ~f`


   - This is `fold_left` without labeled arguments, order matters, like you
     are probably used to
     `List.fold_left ( + ) 0`

   See: https://ocaml.org/docs/labels

   If you don't want this, open Containers instead of ContainersLabels. *)

(* Advent of Code usually provides of with some smaller examples.
   I usually do inline testing to verify that my solution matches the examples.

    OCaml supports multi-line strings by default. *)

(* Paste sample data here *)
let sample = {|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|} |> String.trim

(* If some parts of a solution is used in both Part_1 and Part_2 I place the
   outside the modules *)

(* These are my solutions to Day 1 from 2021: https://adventofcode.com/2021/day/1 *)
module Part_1 = struct
  let is_number c = Char.(c >= '0' && c <= '9')

  let solve input =
    let lines = input |> String.lines in
    let lines = List.map ~f:(String.filter ~f:is_number) lines in
    List.map
      ~f:(fun s ->
        String.sub s ~pos:0 ~len:1
        ^ String.sub s ~pos:(String.length s - 1) ~len:1)
      lines
    |> List.map ~f:int_of_string
    |> List.fold_left ~f:Int.add ~init:0

  (* According to the description the expected value should be 7 given the
     sample data. *)
  let%test "sample data" = Test.(run int (solve sample) ~expect:142)
end

module Part_2 = struct
  let solve input = 5
  let%test "sample data" = Test.(run int (solve sample) ~expect:5)
end

let run_1 () =
  Run.solve_int (module Part_1);
  (* Run.solve_string (module Part_1); *)
  ()

let run_2 () =
  (* When you are done, uncomment this to run the "real thing" *)
  (* Submit the result *)
  (* run `dune promote` *)
  (* Run.solve_int (module Part_2); *)
  (* Run.solve_string (module Part_2); *)
  ()
