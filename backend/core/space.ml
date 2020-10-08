open Utils
open Sdk.UID

let to_json (x,y) = `Assoc [("x", `Int x); ("y", `Int y)]

type 'a t = {
  the_universe: unit -> 'a;
  get_path: UID.t -> UID.t -> UID.t array;
  get_view: HexCoordinate.t -> 'a list;
  get_tile: HexCoordinate.t -> 'a option;
  get_npc: UID.t -> 'a;
  register_event: Timer.slice -> 'a -> unit;
  cancel_event: 'a -> unit;
  set_active: 'a -> unit;
}
