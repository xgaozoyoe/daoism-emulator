open Lwt.Syntax
module Apprentice = Npc.Apprentice
module Creature = Npc.Creature
module Npc = Npc.Api
module TilesApi = Tiles.Api

open Core

module ID = UID.Make(UID.Id)
module ObjectSet = Set.Make (Object)

type config = {
  tile_rule: unit
}

let default_config _ = {tile_rule = ()}

class elt n = object(self)

  inherit Object.elt n (-1,-1)

  val mutable map: TilesApi.map = TilesApi.mk_map 32 16
  val mutable events: Event.t list = []
  val npcs:(ID.t, Object.t) Hashtbl.t = Hashtbl.create 10
  val mutable event_queue = Timer.TriggerQueue.empty
  val mutable update = ObjectSet.empty

  method get_update: Yojson.Basic.t =
    let objs = ObjectSet.fold (fun v acc ->
      acc @ [v#to_json]
    ) update [] in
    `Assoc [ ("updates", `List objs) ]

  method space: Object.t Space.t = let open Space in
  {
    get_path = (fun _ _ -> [||]);
    the_universe = (fun _ -> (self :> Object.t));
    cancel_event = (fun _ -> ());
    register_event = (fun t o ->
      event_queue <- Timer.TriggerQueue.register_event t o event_queue);
    get_view = (fun cor -> TilesApi.get_view cor map);
    get_tile = (fun cor -> TilesApi.get_tile cor map);
    set_active = (fun o -> update <- ObjectSet.add o update);
  }

  method to_json =
    let seq = List.of_seq @@ Hashtbl.to_seq npcs in
    let npcs = List.fold_left (fun acc (_, v) ->
      acc @ [v#to_json]
    ) [] seq in
    `Assoc [
        ("tiles", TilesApi.to_json (map))
      ; ("npcs", `List npcs)
    ]

  method step space = begin
    update <- ObjectSet.empty;
    let* _ = Lwt_io.printf ("step...\n") in
    (* let* _ = Timer.TriggerQueue.dump event_queue (fun x -> x#get_name) in *)
    let* left_evts = Lwt_list.fold_left_s (fun acc e ->
      let target = Event.get_target e in
      let* evts = target#handle_event (self#space) (Event.get_source e) (Event.get_feature e) in
      (* FIXME: might trigger extra events *)
      Lwt.return @@ acc @ (List.map (fun (f,s,t) -> Event.mk_event f s t) evts)
    ) [] events in

    let seq, queue = Timer.TriggerQueue.fetch_events [] event_queue in
    event_queue <- queue;
    let* _ = Lwt_io.printf ("trigger %d objs\n") (List.length seq) in

    let* evts = Lwt_list.fold_left_s (fun acc v ->
      let* es = v#step space in
      let* new_events = Lwt_list.map_s (fun (f, s, t) -> Lwt.return (Event.mk_event f s t)) es in
      Lwt.return @@ new_events @ acc
    ) [] seq in

    let my_events, deliver_events =
      List.partition (fun e -> (Event.get_target e)#get_name = self#get_name)
      evts
    in
    let* _ = Lwt_list.iter_s (fun e -> Logger.log "[ my event: %s ]\n" (Event.to_string e)) my_events in
    let* _ = Lwt_list.iter_s (fun e -> Logger.log "[ deliver event: %s ]\n" (Event.to_string e)) deliver_events in

    (* Handle events that are generated for universe *)
    let* _ = Lwt_list.iter_s (fun e ->
      let feature = Event.get_feature e in
      match feature with
      | Feature.Produce (attr, _) -> begin
          match attr#category with
          | "Spawn" when Population.try_spawn () -> begin
              Population.increase_population ();
              match attr#name with
              | "Apprentice" -> begin
                  let obj = Apprentice.mk_apprentice (Event.get_source e).(0) in
                  Hashtbl.add npcs (ID.of_string obj#get_name) (obj:>Object.t);
                  let x,y = obj#get_loc in
                  let* _ = Lwt_io.printf "spawn at location (%d,%d)\n" x y in
                  Lwt.return @@ space.register_event (Timer.of_int 1) (obj:>Object.t)
                end
              | "Creature" -> begin
                  let obj = Creature.mk_creature (Event.get_source e).(0) in
                  Hashtbl.add npcs (ID.of_string obj#get_name) (obj:>Object.t);
                  let x,y = obj#get_loc in
                  let* _ = Lwt_io.printf "spawn at location (%d,%d)\n" x y in
                  Lwt.return @@ space.register_event (Timer.of_int 1) (obj:>Object.t)
                end
              | _ -> Lwt.return ()
            end
          | _ -> Lwt.return ()
        end
      | Feature.Hold (attr, _) when attr#test "Status" && attr#name = "dead" ->
        let npc = (Event.get_source e).(0) in
        Lwt.return @@ Hashtbl.remove npcs (ID.of_string npc#get_name)
      | _ -> Lwt.return ()
    ) my_events in

    events <- left_evts @ deliver_events;
    Lwt.return []
  end

  method handle_event _ _ _ = Lwt.return []

  method init (_:unit) =
    let space = self#space in
    map <- TilesApi.init_map space map (TileRule.tile_rule self)


end

let init _ =
  let universe = new elt "Universe" in
  universe#init ();
  universe
