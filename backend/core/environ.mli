open Sdk.UID
module Attribute = Attribute.Api

type 'a elt = 'a Attribute.t * int
type 'a rule = ('a elt) array * ('a Feature.t * 'a)
type 'a t

val empty: 'a rule list -> 'a t
val proceed_feature: 'a Feature.t -> 'a t -> unit
val replace_feature: 'a elt -> 'a t -> 'a elt option
val fold: (UID.t -> ('a Attribute.t * int) -> 'a -> 'a)  -> 'a -> 'a t -> 'a
val install_rule: 'a rule -> 'a t -> unit
val apply_rules: 'a t -> ('a Feature.t * 'a) list
val set_bound: ('a Attribute.t * int) -> 'a t -> unit
val dump: 'a t -> string
val to_json: 'a t -> Yojson.Basic.t
val elt_to_json: 'a elt -> Yojson.Basic.t
