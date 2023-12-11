type t

val make : x:int -> y:int -> t
(** [make x y] create a new point with coordinate (x, y). *)

val equal : t -> t -> bool
(** [equal p1 p2] is true with p1 and p2 are equal. *)

val manhattan_distance : t -> t -> int
(** [manhattan_distance p1 p2] is the manhattan distance between p1 and p2. *)

val x : t -> int
val y : t -> int
val show : t -> string
val pp : Format.formatter -> t -> unit
