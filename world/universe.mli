type tile = struct {
  mobs: MobMap.t,
  resources: ResourceMap.t,
  structures: StructureMap.t
}

type time_slice = int

type coordinate = struct {
  x: int,
  y: int,
}

module TileMap : sig
  type t
  get_tile: coordinate -> tile
  get_distance: coordinate -> coordinate -> int
  fold_within: ('a -> tile -> 'a) -> coordinate -> distance -> 'a -> t -> 'a
  filt_within: (tile -> bool) -> coordinate -> distance -> t -> tile list
  fold: ('a tile -> 'a) -> 'a -> t -> 'a
  map: ('a tile -> 'a) -> 'a -> t -> 'a
end

type t = struct {
  map: TileMap.t
  event_pool : EventPool.t
}

type config = struct {
  universe_config: Universe.config
}

val default_config: unit -> t
val init: config -> t
val register_event_handler: EventHandler.t -> unit
val run: t -> time_slice -> unit
val one_step: unit -> (event Event.t) list
