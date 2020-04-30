open Lwt.Syntax
module Apprentice = Npc.Apprentice
module Npc = Npc.Api
module Tiles = Tiles.Api

open Core
module ID = UID.Make(UID.Id)

type config = {
  config: unit
}

let default_config _ = {config = ()}

class elt n = object(self)

  inherit Object.elt n


  val mutable tiles: Tiles.map = Tiles.mk_map 2 2
  val mutable events: Event.t list = []
  val objs:(ID.t, Object.t) Hashtbl.t = Hashtbl.create 10

  method step oref = begin
    let* tile_events = Tiles.step tiles oref in
    let seq = List.of_seq @@ Hashtbl.to_seq objs in
    let* events = Lwt_list.fold_left_s (fun acc (_, v) ->
      let* es = v#step oref in
      let* new_events = Lwt_list.map_s (fun (f, t) -> Lwt.return (Event.mk_event v t f)) es in
      Lwt.return @@ new_events @ acc
    ) [] seq in
    let my_events, deliver_events =
      List.partition (fun e -> (Event.get_target e)#get_name = self#get_name)
      (tile_events @ events) in
    let* _ = Lwt_list.iter_s (fun e -> Logger.log "[ my event: %s ]\n" (Event.to_string e)) my_events in
    let* _ = Lwt_list.iter_s (fun e -> Logger.log "[ deliver event: %s ]\n" (Event.to_string e)) deliver_events in
    List.iter (fun e ->
      let feature = Event.get_feature e in
      match feature with
      | Feature.Produce (attr, _) -> begin
          if attr#category = "Npc" then
            let obj = Apprentice.mk_apprentice (Event.get_source e) in
            Hashtbl.add objs (ID.of_string obj#get_name) obj
          else ()
        end
      | _ -> ()
    ) my_events;
    Lwt.return []
    (* deliver_events *)
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
