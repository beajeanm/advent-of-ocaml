open ContainersLabels

let sample =
  {|
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}
  |> String.trim

module Key = struct
  type t = Range of (int * int) | Dot of int
  [@@deriving show { with_path = false }]

  let compare a b =
    let open Int in
    match (a, b) with
    | Dot d, Range (low, high) ->
        if low <= d && d <= high then 0 else Int.compare d low
    | Range (low, high), Dot d ->
        if low <= d && d <= high then 0 else Int.compare low d
    (* Assumption: ranges are always distinct by construction *)
    | Range (l1, _), Range (l2, _) -> Int.compare l1 l2
    | _ -> failwith "Unexpected"
end

type value = { source : int; destination : int }
[@@deriving show { with_path = false }]

let to_key_value (destination, source, length) =
  (Key.Range (source, source + length - 1), { source; destination })

module KeyTree = CCWBTree.Make (Key)

type almanac = {
  seeds : int list;
  seed_soil : value KeyTree.t;
  soil_fertilizer : value KeyTree.t;
  fertilizer_water : value KeyTree.t;
  water_light : value KeyTree.t;
  light_temperature : value KeyTree.t;
  temperature_humidity : value KeyTree.t;
  humidity_location : value KeyTree.t;
}

let parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let open Util.Parser in
  let mappings =
    let mapping_line =
      both (integer <* whitespaces) (both (integer <* whitespaces) integer)
      >>| fun (a, (b, c)) -> (a, b, c)
    in
    let* _mapping_name =
      take_while (Fun.negate @@ Char.equal ':') <* char ':' <* end_of_line
    in
    let+ lines = sep_by end_of_line mapping_line <* skip_many end_of_line in
    let key_values = List.map ~f:to_key_value lines in
    List.fold_left
      ~f:(fun m (k, v) -> KeyTree.add k v m)
      ~init:KeyTree.empty key_values
  in
  let* seeds =
    string "seeds:" *> whitespaces *> sep_by whitespaces integer
    <* skip_many end_of_line
  in
  let* seed_soil = mappings in
  let* soil_fertilizer = mappings in
  let* fertilizer_water = mappings in
  let* water_light = mappings in
  let* light_temperature = mappings in
  let* temperature_humidity = mappings in
  let+ humidity_location = mappings in
  {
    seeds;
    seed_soil;
    soil_fertilizer;
    fertilizer_water;
    water_light;
    light_temperature;
    temperature_humidity;
    humidity_location;
  }

let get_mapping map key =
  let open Key in
  match key with
  | Dot d as dot ->
      KeyTree.get key map
      |> Option.map (fun v -> Dot (v.destination - v.source + d))
      |> Option.get_or ~default:dot
  | Range _ -> failwith "Range in get mapping"

let seed_to_location almanac seed =
  let mapping =
    get_mapping almanac.seed_soil (Dot seed)
    |> get_mapping almanac.soil_fertilizer
    |> get_mapping almanac.fertilizer_water
    |> get_mapping almanac.water_light
    |> get_mapping almanac.light_temperature
    |> get_mapping almanac.temperature_humidity
    |> get_mapping almanac.humidity_location
  in
  match mapping with
  | Dot d -> d
  | Range _ -> failwith "Range in seed to location"

module Part_1 = struct
  let solve input =
    let almanac =
      Angstrom.parse_string ~consume:All parser input |> Result.get_exn
    in
    let locations = List.map ~f:(seed_to_location almanac) almanac.seeds in
    List.fold_left ~f:Int.min ~init:Int.max_int locations

  let%test "sample data" = Test.(run int (solve sample) ~expect:35)
end

module Part_2 = struct
  let solve input =
    let open Iter.Infix in
    let almanac =
      Angstrom.parse_string ~consume:All parser input |> Result.get_exn
    in
    let seed_ranges = Array.of_list almanac.seeds in
    let len = Array.length seed_ranges in
    let all_seeds =
      0 -- ((len / 2) - 1)
      |> Iter.flat_map (fun i ->
             let start = seed_ranges.(i * 2) in
             let length = seed_ranges.((i * 2) + 1) in
             start -- (start + length))
    in
    Iter.map (seed_to_location almanac) all_seeds
    |> Iter.fold Int.min Int.max_int

  let%test "sample data" = Test.(run int (solve sample) ~expect:46)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Takes about 5 minutes on a single core, but who cares... *)
  Run.solve_int (module Part_2);
  ()
