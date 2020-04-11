module UID = UID.Make (UID.Id)

module Feature = Feature.Make (UID)

module Modifier = Modifier.Make (UID) (Feature)

module Env = Environ.Make (UID) (Feature) (Modifier)

module Object = Object.Make (UID) (Env) (Modifier)

module Tile = Tile.Make (Object)

module Event = Event.Make (Object) (Feature)

module Npc = Npc.Make (Object)

type universe = {
    tile: Tile.map;
    events: Event.t list
}

type config = unit

let default_config _ = ()

let init _ = { tile = Tile.mkMap (); events = [] }

(* run univ t *)
let run _ _ = ()

let one_step _ = []
