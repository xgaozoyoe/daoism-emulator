open Core

type map
type tile_state
val mk_map: int -> int -> map
val get_tile: Space.coordinate -> map -> Object.t option
val step: map -> Object.t -> Object.t Space.t -> Event.t list Lwt.t
