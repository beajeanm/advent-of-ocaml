open ContainersLabels

let sample =
  {|
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
|} |> String.trim

let hash =
  let aux h c = (h + Char.code c) * 17 mod 256 in
  String.fold_left ~f:aux ~init:0

let parse_part_1 = String.split_on_char ~by:','

type lens = { label : string; focal : int }
[@@deriving show { with_path = false }]

type step = Add of lens | Remove of string
[@@deriving show { with_path = false }]

let hash_lens s = hash s.label
let equal_lens s1 s2 = String.equal s1.label s2.label

module IntMap = Hashtbl.Make (Int)

let parse_part_2 input =
  let open Angstrom in
  let open Util.Parser in
  let step_parser =
    let label = take_till (fun c -> Char.equal '-' c || Char.equal '=' c) in
    let add =
      both label (char '=' *> integer)
      |> map ~f:(fun (label, focal) -> Add { label; focal })
    in
    let remove = label <* char '-' |> map ~f:(fun label -> Remove label) in
    add <|> remove
  in
  let parse_step input =
    parse_string ~consume:All step_parser input |> Result.get_exn
  in
  parse_part_1 input |> List.map ~f:parse_step

let remove_lens boxes label =
  let hash = hash label in
  let box = IntMap.find_opt boxes hash |> Option.value ~default:[] in
  let updated_box =
    List.filter ~f:(fun l -> not (String.equal label l.label)) box
  in
  IntMap.replace boxes hash updated_box

let add_lens boxes lens =
  let hash = hash_lens lens in
  let box = IntMap.find_opt boxes hash |> Option.value ~default:[] in
  let is_in_box = List.mem ~eq:equal_lens lens box in
  let updated_box =
    if is_in_box then
      List.map
        ~f:(fun l -> if String.equal l.label lens.label then lens else l)
        box
    else lens :: box
  in
  IntMap.replace boxes hash updated_box

let process_step boxes = function
  | Remove label -> remove_lens boxes label
  | Add lens -> add_lens boxes lens

let focusing_power box_num index lens = (box_num + 1) * index * lens.focal

let process_box box_num box =
  List.rev box
  |> List.foldi
       ~f:(fun acc i lens -> acc + focusing_power box_num (i + 1) lens)
       ~init:0

module Part_1 = struct
  let solve input = parse_part_1 input |> List.map ~f:hash |> Util.sum
  let%test "sample data" = Test.(run int (solve sample) ~expect:1320)
end

module Part_2 = struct
  let solve input =
    let steps = parse_part_2 input in
    let boxes = IntMap.create 256 in
    List.iter ~f:(process_step boxes) steps;
    IntMap.to_seq boxes
    |> Seq.fold_left (fun acc (box_num, box) -> acc + process_box box_num box) 0

  let%test "sample data" = Test.(run int (solve sample) ~expect:145)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
