open Core

type coordinate
type tile
type map = (tile option) array

val mk_map: int -> int -> map
val mk_tile: string -> tile
val step: map -> Object.t ref -> (Feature.t * Object.t ref) list
