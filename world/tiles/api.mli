open Core

type map

val mk_map: int -> int -> map
val init_map: map -> (Default.tile_type -> ((Feature.t * Object.t) Environ.rule) array) -> map
val get_tile: Space.coordinate -> map -> Object.t option
val step: map -> Object.t -> Object.t Space.t -> Event.t list Lwt.t
val to_json: map -> Yojson.Basic.t
