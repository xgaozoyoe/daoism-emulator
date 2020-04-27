open Core

type coordinate
type tile = Object.t
type map

val mk_cor: int -> int -> coordinate
val mk_map: int -> int -> map
val get_tile: coordinate -> map -> tile option
val step: map -> Object.t -> Event.t list
