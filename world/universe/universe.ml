open Core

module UID = UID.Make (UID.Id)

module Feature = Feature.Make (UID)

module Modifier = Modifier.Make (UID) (Feature)

module Environ = Environ.Make (UID) (Feature) (Modifier)

module Object = Object.Make (UID) (Environ)

module Event = Event.Make (Object)

module Apprentice = Npc.Apprentice.Make (Object)

module Npc = Npc.Api.Make (Object)

module Tiles = Tiles.Api.Make (Object)

type universe = {
    tile: Tiles.map;
    events: Event.t list;
    objs: (UID.t, Object.t ref) Hashtbl.t
}

type config = unit

let default_config _ = ()

let init _ = {
    tile = Tiles.mk_map 10 10;
    events = [];
    objs = Hashtbl.create 10;
}

let step univ : Event.t list =
  Tiles.step univ.tile;
  Hashtbl.fold (fun _ v acc ->
    let new_events = List.map (fun (f, t) -> Event.mk_event v t f) (!v#step ()) in
    new_events @ acc
  ) univ.objs []

