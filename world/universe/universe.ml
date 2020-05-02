open Lwt.Syntax
module Apprentice = Npc.Apprentice
module Npc = Npc.Api
module TilesApi = Tiles.Api

open Core
module ID = UID.Make(UID.Id)

type config = {
  tile_rule: unit
}

let default_config _ = {tile_rule = ()}

class elt n = object(self)

  inherit Object.elt n

  val mutable tiles: TilesApi.map = TilesApi.mk_map 2 2
  val mutable events: Event.t list = []
  val npcs:(ID.t, Object.t) Hashtbl.t = Hashtbl.create 10

  method space: Object.t Space.t = let open Space in
  {
    pick_from_coordinate = (fun x -> TilesApi.get_tile x tiles);
    get_path = fun _ _ -> [||]
  }

  method to_json =
    let seq = List.of_seq @@ Hashtbl.to_seq npcs in
    let npcs = List.fold_left (fun acc (_, v) ->
      acc @ [(v#get_name, v#to_json)]
    ) [] seq in
    `Assoc [
        ("Tiles", TilesApi.to_json (tiles))
      ; ("Npcs", `Assoc npcs)
    ]

  method step oref space = begin
    let* left_evts = Lwt_list.fold_left_s (fun acc e ->
      let target = Event.get_target e in
      let* evts = target#handle_event (self :> Object.t) (Event.get_source e) (Event.get_feature e) in
      (* FIXME: might trigger extra events *)
      Lwt.return @@ acc @ (List.map (fun (f,s,t) -> Event.mk_event f s t) evts)
    ) [] events in

    let* tile_events = TilesApi.step tiles oref space in
    let seq = List.of_seq @@ Hashtbl.to_seq npcs in
    let* evts = Lwt_list.fold_left_s (fun acc (_, v) ->
      let* es = v#step oref space in
      let* new_events = Lwt_list.map_s (fun (f, s, t) -> Lwt.return (Event.mk_event f s t)) es in
      Lwt.return @@ new_events @ acc
    ) [] seq in
    let my_events, deliver_events =
      List.partition (fun e -> (Event.get_target e)#get_name = self#get_name)
      (tile_events @ evts) in
    let* _ = Lwt_list.iter_s (fun e -> Logger.log "[ my event: %s ]\n" (Event.to_string e)) my_events in
    let* _ = Lwt_list.iter_s (fun e -> Logger.log "[ deliver event: %s ]\n" (Event.to_string e)) deliver_events in
    List.iter (fun e ->
      let feature = Event.get_feature e in
      match feature with
      | Feature.Produce (attr, _) -> begin
          if attr#category = "Spawn" && Hashtbl.length npcs < 3 then
            let obj = Apprentice.mk_apprentice (Event.get_source e).(0) in
            Hashtbl.add npcs (ID.of_string obj#get_name) (obj:>Object.t)
          else ()
        end
      | Feature.Hold (attr, _) when attr#test "Status" && attr#name = "dead" ->
        let npc = (Event.get_source e).(0) in
        Hashtbl.remove npcs (ID.of_string npc#get_name)
      | _ -> ()
    ) my_events;
    events <- left_evts @ deliver_events;
    Lwt.return []
  end

  method handle_event _ _ _ = Lwt.return []

  method init (_:unit) =
    tiles <- TilesApi.init_map tiles (TileRule.tile_rule self)


end

let init _ =
  let universe = new elt "Universe" in
  universe#init ();
  universe
