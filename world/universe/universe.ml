open Core

module Npc = Npc.Api

module Tiles = Tiles.Api

type config = unit

let default_config _ = ()

class elt n = object (self)

  inherit Object.elt n

  val mutable tiles: Tiles.map = Tiles.mk_map 10 10
  val mutable events: Event.t list = []
  val objs:(Object.ID.t, Object.t ref) Hashtbl.t = Hashtbl.create 10

  method step oref = begin
    Tiles.step tiles oref;
    Hashtbl.fold (fun _ v acc ->
      let new_events = List.map (fun (f, t) -> Event.mk_event v t f) (!v#step ()) in
      new_events @ acc
    ) univ.objs []
  end
end

let init _ = new elt "Universe"

let step univ : Event.t list = univ#step univ
