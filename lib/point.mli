type t = { x : int; y : int } [@@deriving eq, show, make]

val manhattan_distance : t -> t -> int
(** [manhattan_distance p1 p2] is the manhattan distance between p1 and p2. *)

val down : t -> t
val up : t -> t
val right : t -> t
val left : t -> t
val data : 'a array array -> t -> 'a
val safe_get : 'a array array -> t -> 'a option
val hash : t -> int
