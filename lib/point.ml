type t = { x : int; y : int } [@@deriving eq, show, make]

let manhattan_distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)
let x t = t.x
let y t = t.y
