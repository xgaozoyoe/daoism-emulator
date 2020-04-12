open Core

module UID = UID.Make (UID.Id)

module Feature = Feature.Make (UID)

module Modifier = Modifier.Make (UID) (Feature)

module Environ = Environ.Make (UID) (Feature) (Modifier)

module Object = Object.Make (UID) (Environ) (Modifier)

module Event = Event.Make (Object)

module Tile = Tile.Make (Object)

module Apprentice = Npc.Apprentice.Make (Object)

module Npc = Npc.Api.Make (Object)


type universe = {
    tile: Tile.map;
    events: Event.t list;
    objs: (UID.t, Object.t ref) Hashtbl.t
}

type config = unit

let default_config _ = ()

let init _ = {
    tile = Tile.mk_map 10 10;
    events = [];
    objs = Hashtbl.create 10;
}

let step univ : Event.t list =
  Hashtbl.fold (fun _ v acc ->
    let new_events = List.map (fun t -> Event.mk_event v t) (Object.step !v) in
    new_events @ acc
  ) univ.objs []
