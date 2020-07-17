open Core
open Utils

type map

val mk_map: int -> int -> map
val init_map: Object.t Space.t -> map -> (Default.tile_type -> ((Feature.t * Object.t) Environ.rule) array) -> map
val get_tile: HexCoordinate.t -> map -> Object.t option
val get_view: HexCoordinate.t -> map -> Object.t list
val to_json: map -> Yojson.Basic.t

