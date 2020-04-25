module ID:UID.Interface

type elt = Attribute.t * int
type 'a rule
type 'a t

val empty: 'a t
val proceed_feature: Feature.t -> 'a t -> unit
val fold: (ID.t -> (Attribute.t * int) -> 'a -> 'a)  -> 'a -> 'a t -> 'a
val apply_rules: 'a t -> 'a list
val dump: 'a t -> string
