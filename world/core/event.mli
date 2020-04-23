type t

val mk_event: Object.t ref -> Object.t ref -> Feature.t -> t
val get_source: t -> Object.t
val get_feature: t -> Feature.t
val get_target: t -> Object.t
val to_string: t -> string
