type 'a t

val mk_event: 'a Feature.t -> 'a -> 'a -> 'a t
val get_source: 'a t -> 'a
val get_feature: 'a t -> 'a Feature.t
val get_target: 'a t -> 'a

val log_string: ('a -> string) -> 'a t -> string
