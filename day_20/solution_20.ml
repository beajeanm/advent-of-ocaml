open ContainersLabels

let sample =
  {|
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
|}
  |> String.trim

let sample_2 =
  {|
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
|}
  |> String.trim

type pulse = Low | High [@@deriving eq, show { with_path = false }]
type flip_flop_state = On | Off [@@deriving eq, show { with_path = false }]

type module_state =
  | Broadcast of string list
  | Flip_Flop of flip_flop_state * string list
  | Conjunction of (string * pulse) list * string list
[@@deriving eq, show { with_path = false }]

type module' = string * module_state [@@deriving eq]
type command = string * pulse [@@deriving eq, show { with_path = false }]

let parser =
  let open Angstrom in
  let open Util.Parser in
  let name_parser = take_while1 (fun c -> Char.(c >= 'a' && c <= 'z')) in
  let links = sep_by (string ", ") name_parser in
  let broadcaster =
    string "broadcaster -> " *> links
    |> map ~f:(fun l -> ("broadcaster", Broadcast l))
  in
  let flip_flop =
    let* name = char '%' *> name_parser <* string " -> " in
    let+ links = links in
    (name, Flip_Flop (Off, links))
  in
  let conjunction =
    let* name = char '&' *> name_parser <* string " -> " in
    let+ links = links in
    (name, Conjunction ([], links))
  in
  let row_parser = choice [ broadcaster; flip_flop; conjunction ] in
  sep_by end_of_line row_parser

let parse input =
  Angstrom.parse_string ~consume:All parser input |> Result.get_exn

let to_commands pulse = List.map ~f:(fun name -> (name, pulse))

let process_pulse sender current pulse =
  match (current, pulse) with
  | Broadcast _, High -> failwith "High on broadcast"
  | Broadcast modules, Low -> (current, to_commands Low modules)
  | Flip_Flop _, High -> (current, [])
  | Flip_Flop (Off, modules), Low ->
      (Flip_Flop (On, modules), to_commands High modules)
  | Flip_Flop (On, modules), Low ->
      (Flip_Flop (Off, modules), to_commands Low modules)
  | Conjunction (states, modules), _ ->
      let updated_states =
        List.Assoc.set ~eq:String.equal sender pulse states
      in
      let all_highs =
        List.for_all
          ~f:(fun (_, pulse) -> equal_pulse High pulse)
          updated_states
      in
      let pulse_to_send = if all_highs then Low else High in
      (Conjunction (updated_states, modules), to_commands pulse_to_send modules)

let push_button modules =
  let queue = CCDeque.create () in
  let low_count = ref 0 in
  let high_count = ref 0 in
  let add_to_queue target sender pulse =
    if equal_pulse High pulse then incr high_count else incr low_count;
    CCDeque.push_back queue (target, (sender, pulse))
  in
  add_to_queue "broadcaster" "Unknown" Low;
  let current_state = ref modules in
  while not (CCDeque.is_empty queue) do
    let target, (sender, pulse) = CCDeque.take_front queue in
    let module_to_process =
      List.Assoc.get ~eq:String.equal target !current_state
    in
    if Option.is_some module_to_process then (
      let update_module, next_pulses =
        process_pulse sender (Option.get_exn_or "WTF" module_to_process) pulse
      in
      List.Assoc.get ~eq:String.equal "rx" next_pulses
      |> Option.iter (fun p ->
             if equal_pulse Low p then
               Format.printf "Low pulsed delivered to RX!!!!\n"
             else ());
      Ref.update
        (fun states ->
          List.Assoc.set ~eq:String.equal target update_module states)
        current_state;
      List.iter
        ~f:(fun (name, pulse) -> add_to_queue name target pulse)
        next_pulses)
    else ()
  done;
  (!current_state, !low_count, !high_count)

let update_conjunctions modules =
  let update_conjunction connected = function
    | Conjunction ([], links) ->
        let connected_state = List.map ~f:(fun name -> (name, Low)) connected in
        Conjunction (connected_state, links)
    | _ -> failwith "update_conjunction"
  in
  let is_conjunction = function Conjunction _ -> true | _ -> false in
  let is_linked_to name = function
    | Broadcast links -> List.mem ~eq:String.equal name links
    | Flip_Flop (_, links) -> List.mem ~eq:String.equal name links
    | Conjunction (_, links) -> List.mem ~eq:String.equal name links
  in
  let linked_modules name =
    List.filter ~f:(fun (_, mod_state) -> is_linked_to name mod_state) modules
    |> List.Assoc.keys
  in
  let conjunctions =
    List.filter ~f:(fun (_, mod_state) -> is_conjunction mod_state) modules
    |> List.Assoc.keys
  in
  List.fold_left
    ~f:(fun state name ->
      let linked = linked_modules name in
      List.Assoc.set ~eq:String.equal name
        (update_conjunction linked
        @@ List.Assoc.get_exn ~eq:String.equal name state)
        state)
    ~init:modules conjunctions

let push_button_n_times n modules =
  let modules = update_conjunctions modules in
  let open Seq.Infix in
  let _, low, high =
    0 --^ n
    |> Seq.fold_left
         (fun (state, low, high) _ ->
           let new_state, inc_low, inc_high = push_button state in
           (new_state, inc_low + low, inc_high + high))
         (modules, 0, 0)
  in
  low * high

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    push_button_n_times 1000 parsed

  let%test "sample data" = Test.(run int (solve sample) ~expect:32000000)
  let%test "sample data 2" = Test.(run int (solve sample_2) ~expect:11687500)
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
