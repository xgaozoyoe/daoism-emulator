module Attribute = Attribute.Api
type 'a t =
  | Consume of 'a Attribute.t * int
  | Produce of 'a Attribute.t * int
  | Hold of 'a Attribute.t * int

val mk_produce: 'a Attribute.t -> int -> 'a t
val mk_consume: 'a Attribute.t -> int -> 'a t
val mk_hold: 'a Attribute.t -> int -> 'a t

val to_string: 'a t -> string
val to_json: 'a t -> Yojson.Basic.t

