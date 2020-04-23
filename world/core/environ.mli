module ID:UID.Interface

type t
type elt = Attribute.t * int

val empty: t
val proceed_feature: Feature.t -> t -> unit
val fold: (ID.t -> (Attribute.t * int) -> 'a -> 'a)  -> 'a -> t -> 'a
val apply_rules: t -> Feature.t list
