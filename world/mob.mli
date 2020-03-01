type 'a t

include Obj
val move: ref t -> Coordinate.t -> unit
val consume: ref t -> Resource.t -> unit
val attack: ref t -> ref t -> unit
val speak: ref t -> unit
val improve: ref t -> unit
