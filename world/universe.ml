open Timer
open Event
open Object

module UID = UID.Make (String)

module Feature = Feature.Make (UID)

module Object = Object.Make ()

module Tile = Tile.Make (Object)

module Event = Event.Make (Object) (Feature)

type universe = {
    tile: Tile.map;
    events: Event.t list
}

type config = unit

let default_config _ = ()

let init _ = { tile = Tile.mkMap (); events = [] }

let run univ t = ()

let one_step _ = []
