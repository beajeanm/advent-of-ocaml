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

type op = GT | LT [@@deriving show { with_path = false }]

type action = Rejected | Accepted | Workflow of string
[@@deriving show { with_path = false }]

type category = Extremely_cool | Musical | Aerodynamic | Shiny
[@@deriving show { with_path = false }]

type rule = Next of category * op * int * action | Terminate of action
[@@deriving show { with_path = false }]

type workflow = string * rule list [@@deriving show { with_path = false }]

type rating = { cool : int; musical : int; aero : int; shiny : int }
[@@deriving show { with_path = false }]

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

let is_accepted workflows rating =
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

type rating_range = {
  cool_range : int * int;
  musical_range : int * int;
  aero_range : int * int;
  shiny_range : int * int;
}
[@@deriving show { with_path = false }]

let generate_ranges workflows =
  (* Get the rating range for the given category *)
  let get_rating_range range = function
    | Extremely_cool -> range.cool_range
    | Musical -> range.musical_range
    | Aerodynamic -> range.aero_range
    | Shiny -> range.shiny_range
  in
  (* Update the range of the given category *)
  let update_range range low high = function
    | Extremely_cool -> { range with cool_range = (low, high) }
    | Musical -> { range with musical_range = (low, high) }
    | Aerodynamic -> { range with aero_range = (low, high) }
    | Shiny -> { range with shiny_range = (low, high) }
  in
  (* Easier to eliminate wrong range here rather than have messy coditions above. *)
  let sage_update_range range low high category =
    if low > high then None else Some (update_range range low high category)
  in
  (*
     Split the given range in 2.
     Given a range [a, b] and a rule '< l' with l between a and b, then the new
     ranges are Some [a, l-1] and Some [l, b].
     if l is greater than b then Some [a, b], None
     if l is less than a then None, Some [a, b]
     The rules are mirrored for '> l'
  *)
  let split range category op limit =
    let low, high = get_rating_range range category in
    match op with
    | GT ->
        let upped_range_limit = max (limit + 1) low in
        ( sage_update_range range upped_range_limit high category,
          sage_update_range range low (upped_range_limit - 1) category )
    | LT ->
        let lower_range_limit = min (limit - 1) high in
        ( sage_update_range range low lower_range_limit category,
          sage_update_range range (lower_range_limit + 1) high category )
  in
  (* what to do with a range given an action. *)
  let rec range_for_action range = function
    (* We reject the whole range (e.g. 0 combination) *)
    | Rejected -> []
    (* Accept the range as provided. *)
    | Accepted -> [ range ]
    (* For workflow, we need to process each rule in order.
       The condition of a rule split the current range between a A range that
       satisfies the condition and it's complement B.
       Range A is further refined by processing the action associated with the rule.
       Range B is used as the starting range for the next rule in the worflow list.
       The valid range for this workflow is the concatenation of all the ranges obtained
       by process its rules with the process above.
    *)
    | Workflow name ->
        let rules = List.assoc ~eq:String.equal name workflows in
        process_rules range rules
  and process_rules range rules =
    match rules with
    | Terminate workflow :: [] -> range_for_action range workflow
    | Next (category, op, limit, action) :: tl ->
        let (updated_range, complement) :
            rating_range option * rating_range option =
          split range category op limit
        in
        let first_part =
          Option.map (fun r -> range_for_action r action) updated_range
          |> Option.value ~default:[]
        in
        let second_part =
          Option.map (fun r -> process_rules r tl) complement
          |> Option.value ~default:[]
        in
        List.concat [ first_part; second_part ]
        (* We shouldn't reach this unless parsing is wrong. *)
    | _ -> failwith (Format.sprintf "Process rules: %a" (List.pp pp_rule) rules)
  in
  let start =
    {
      cool_range = (1, 4000);
      musical_range = (1, 4000);
      aero_range = (1, 4000);
      shiny_range = (1, 4000);
    }
  in
  range_for_action start (Workflow "in")

let count_combinations { cool_range; musical_range; aero_range; shiny_range } =
  let nb (low, high) = high - low + 1 in
  nb cool_range * nb musical_range * nb aero_range * nb shiny_range

module Part_1 = struct
  let solve input =
    let workflows, ratings = parse input in
    let accepted = List.filter ~f:(is_accepted workflows) ratings in
    List.map ~f:sum_rating accepted |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:19114)
end

module Part_2 = struct
  let solve input =
    let workflows, _ = parse input in
    let ranges = generate_ranges workflows in
    List.map ~f:count_combinations ranges |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:167409079868000)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
