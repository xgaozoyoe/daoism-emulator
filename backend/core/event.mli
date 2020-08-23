type t

val mk_event: Object.t Feature.t -> Object.t array -> Object.t -> t
val get_source: t -> Object.t array
val get_feature: t -> Object.t Feature.t
val get_target: t -> Object.t
val to_string: t -> string
