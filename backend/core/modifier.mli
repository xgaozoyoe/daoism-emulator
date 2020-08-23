type 'a t
val mk_modifier: ('a Feature.t -> bool) -> ('a Feature.t list -> int -> int) -> 'a t
