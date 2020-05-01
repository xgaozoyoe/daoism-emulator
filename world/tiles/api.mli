open Core

type coordinate
type map

val mk_cor: int -> int -> coordinate
val mk_map: int -> int -> map
val get_tile: coordinate -> map -> Object.t option
val step: map -> Object.t -> Event.t list Lwt.t
val mk_tile_attr: Object.t -> Attribute.t
