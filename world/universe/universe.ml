open Core

module Npc = Npc.Api

module Tiles = Tiles.Api

type config = {
  config: unit
}

let default_config _ = {config = ()}

class elt n = object

  inherit Object.elt n

  val mutable tiles: Tiles.map = Tiles.mk_map 2 2
  val mutable events: Event.t list = []
  val objs:(Object.ID.t, Object.t ref) Hashtbl.t = Hashtbl.create 10

  method step oref = begin
    ignore @@ Tiles.step tiles oref;
    let events = Hashtbl.fold (fun _ v acc ->
      let new_events = List.map (fun (f, t) -> Event.mk_event v t f) (!v#step oref) in
      new_events @ acc
    ) objs [] in
    List.iter (fun e -> Printf.printf "[event:] %s\n" (Event.to_string e)) events;
    []
  end
end

let init _ = new elt "Universe"

let step univ : Event.t list = univ#step univ
