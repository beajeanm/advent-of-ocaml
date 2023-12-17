open Containers

type t = { x : int; y : int } [@@deriving eq, show, make]

let manhattan_distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)
let down p = { p with x = p.x + 1 }
let up p = { p with x = p.x - 1 }
let right p = { p with y = p.y + 1 }
let left p = { p with y = p.y - 1 }
let data g p = g.(p.x).(p.y)

let safe_get g p =
  Array.get_safe g p.x |> Option.flat_map (fun row -> Array.get_safe row p.y)

let hash t = (CCHash.pair CCHash.int CCHash.int) (t.x, t.y)
let show t = Format.sprintf "(%i, %i)" t.x t.y
