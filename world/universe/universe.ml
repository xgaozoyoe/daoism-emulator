
module Apprentice = Npc.Apprentice
module Npc = Npc.Api
module Tiles = Tiles.Api

open Core

type config = {
  config: unit
}

let default_config _ = {config = ()}

class elt n = object(self)

  inherit Object.elt n


  val mutable tiles: Tiles.map = Tiles.mk_map 2 2
  val mutable events: Event.t list = []
  val objs:(Object.ID.t, Object.t) Hashtbl.t = Hashtbl.create 10

  method step oref = begin
    ignore @@ Tiles.step tiles oref;
    let events = Hashtbl.fold (fun _ v acc ->
      let new_events = List.map (fun (f, t) -> Event.mk_event v t f) (v#step oref) in
      new_events @ acc
    ) objs [] in
    List.iter (fun e -> Printf.printf "[event:] %s\n" (Event.to_string e)) events;
    []
  end

  method init (_:unit) =
    let rule = Apprentice.apprentice_rule (self :> Object.t) in
    let tile = Tiles.get_tile (Tiles.mk_cor 0 0) tiles in
    ignore @@ Option.map (fun tile -> Environ.install_rule rule tile#get_env) tile;

end

let init _ : Object.t =
  let universe = new elt "Universe" in
  universe#init ();
  (universe :> Object.t)

let step univ : Event.t list = univ#step univ
