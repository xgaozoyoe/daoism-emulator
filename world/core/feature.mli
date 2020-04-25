type t =
  | Consume of Attribute.t * int
  | Produce of Attribute.t * int
  | Hold of Attribute.t * int

val mk_produce: Attribute.t -> int -> t
val mk_consume: Attribute.t -> int -> t
val mk_hold: Attribute.t -> int -> t

val to_string: t -> string

