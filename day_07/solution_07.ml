open ContainersLabels

type card =
  | A
  | K
  | Q
  | J
  | T
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
[@@deriving show { with_path = false }, eq, ord]

let parse_card = function
  | 'A' -> A
  | 'K' -> K
  | 'Q' -> Q
  | 'J' -> J
  | 'T' -> T
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Two
  | _ -> failwith "invalid card"

type hand =
  | Five_of_kind of card list
  | Four_of_kind of card list
  | Full_house of card list
  | Three_of_kind of card list
  | Two_pair of card list
  | One_pair of card list
  | High_card of card list
[@@deriving show { with_path = false }, eq, ord]

let hand_rank = function
  | Five_of_kind _ -> 1
  | Four_of_kind _ -> 2
  | Full_house _ -> 3
  | Three_of_kind _ -> 4
  | Two_pair _ -> 5
  | One_pair _ -> 6
  | High_card _ -> 7

let detect_hand cards =
  let grouped = List.group_by ~eq:equal_card cards in
  let num_groups = List.length grouped in
  let largest_group =
    List.fold_left ~f:Int.max ~init:Int.min_int
      (List.map ~f:List.length grouped)
  in
  if largest_group = 5 then Five_of_kind cards
  else if largest_group = 4 then Four_of_kind cards
  else if largest_group = 3 && num_groups = 2 then Full_house cards
  else if largest_group = 3 then Three_of_kind cards
  else if num_groups = 3 then Two_pair cards
  else if num_groups = 4 then One_pair cards
  else High_card cards

type hand_and_bid = { hand : hand; bid : int }
[@@deriving show { with_path = false }]

let sample =
  {|
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|} |> String.trim

let parse line =
  let parse_cards cards =
    String.to_seq cards |> Seq.map parse_card |> Seq.to_list
  in
  Scanf.sscanf line "%s %i" (fun cards bid ->
      { hand = detect_hand (parse_cards cards); bid })

module Part_1 = struct
  let solve input =
    let lines = String.lines input in
    let data = List.map ~f:parse lines in
    (* Flip a and b since we give a lower rank for stronger the card/hand *)
    let sorted_data =
      List.sort ~cmp:(fun a b -> compare_hand b.hand a.hand) data
    in
    List.mapi
      ~f:(fun rank hand_and_bid -> (rank + 1) * hand_and_bid.bid)
      sorted_data
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:6440)
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
