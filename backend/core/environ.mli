module ID:UID.Interface
module Attribute = Attribute.Api

type 'a elt = 'a Attribute.t * int
type 'a rule = ('a elt) array * ('a Feature.t * 'a)
type 'a t

val empty: 'a rule list -> 'a t
val proceed_feature: 'a Feature.t -> 'a t -> unit
val fold: (ID.t -> ('a Attribute.t * int) -> 'a -> 'a)  -> 'a -> 'a t -> 'a
val install_rule: 'a rule -> 'a t -> unit
val apply_rules: 'a t -> ('a Feature.t * 'a) list
val set_bound: ('a Attribute.t * int) -> 'a t -> unit
val dump: 'a t -> string
val to_json: 'a t -> Yojson.Basic.t
