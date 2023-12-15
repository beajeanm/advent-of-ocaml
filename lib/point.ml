type t = { x : int; y : int } [@@deriving eq, show, make]

let manhattan_distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)
let down p = { p with x = p.x + 1 }
let up p = { p with x = p.x - 1 }
let right p = { p with y = p.y + 1 }
let left p = { p with y = p.y - 1 }
