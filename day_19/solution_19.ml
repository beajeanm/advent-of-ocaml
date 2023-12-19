open ContainersLabels

let sample =
  {|
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
|}
  |> String.trim

type op = GT | LT
type action = Rejected | Accepted | Workflow of string
type category = Extremely_cool | Musical | Aerodynamic | Shiny
type rule = Next of category * op * int * action | Terminate of action
type workflow = string * rule list
type rating = { cool : int; musical : int; aero : int; shiny : int }

let sum_rating { cool; musical; aero; shiny } = cool + musical + aero + shiny

let workflow_parser =
  let open Angstrom in
  let open Util.Parser in
  let gt = char '>' *> return GT in
  let lt = char '<' *> return LT in
  let op = gt <|> lt in
  let category =
    choice
      [
        char 'a' *> return Aerodynamic;
        char 'x' *> return Extremely_cool;
        char 'm' *> return Musical;
        char 's' *> return Shiny;
      ]
  in
  let accepted = char 'A' *> return Accepted in
  let rejected = char 'R' *> return Rejected in
  let workflow_name = take_while1 (fun c -> Char.(c >= 'a' && c <= 'z')) in
  let action =
    choice [ accepted; rejected; workflow_name |> map ~f:(fun s -> Workflow s) ]
  in
  let next_rule =
    let* category = category in
    let* op = op in
    let* limit = integer in
    let+ action = char ':' *> action in
    Next (category, op, limit, action)
  in
  let terminte_rule = action |> map ~f:(fun a -> Terminate a) in
  let rules = sep_by (char ',') (next_rule <|> terminte_rule) in
  let* name = workflow_name <* char '{' in
  let+ rules = rules <* char '}' in
  (name, rules)

let rating_parser =
  let open Angstrom in
  let open Util.Parser in
  let* cool = string "{x=" *> integer in
  let* musical = string ",m=" *> integer in
  let* aero = string ",a=" *> integer in
  let+ shiny = string ",s=" *> integer <* string "}" in
  { cool; musical; aero; shiny }

let parse input =
  let open Angstrom in
  let groups = String.split ~by:"\n\n" input in
  let workflows =
    parse_string ~consume:All
      (sep_by end_of_line workflow_parser)
      (List.hd groups)
    |> Result.get_exn
  in
  let ratings =
    parse_string ~consume:All
      (sep_by end_of_line rating_parser)
      (List.tl groups |> List.hd)
    |> Result.get_exn
  in
  (workflows, ratings)

let is_accepted (workflows : (string * rule list) list) rating =
  let get_rating = function
    | Extremely_cool -> rating.cool
    | Musical -> rating.musical
    | Aerodynamic -> rating.aero
    | Shiny -> rating.shiny
  in
  let test rating op limit =
    match op with GT -> rating > limit | LT -> rating < limit
  in
  let matching_rule = function
    | Terminate _ -> true
    | Next (category, op, limit, _) -> test (get_rating category) op limit
  in
  let rec next_actions rules =
    match List.filter ~f:matching_rule rules |> List.hd with
    | Terminate a -> a
    | Next (_, _, _, a) -> a
  in
  let rec find_last_action action =
    match action with
    | Accepted -> true
    | Rejected -> false
    | Workflow name ->
        let rules = List.assoc ~eq:String.equal name workflows in
        find_last_action (next_actions rules)
  in
  find_last_action (Workflow "in")

module Part_1 = struct
  let solve input =
    let workflows, ratings = parse input in
    let accepted = List.filter ~f:(is_accepted workflows) ratings in
    List.map ~f:sum_rating accepted |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:19114)
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
