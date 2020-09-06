open Lwt.Syntax
module Apprentice = Npc.Apprentice
module Creature = Npc.Creature
module Tile = Tiles.Api
open Utils
open Core

module ID = UID.Make(UID.Id)
module ObjectSet = Set.Make (Object)

type config = {
  tile_rule: unit;
  map_width: int;
  map_height: int;
}

let default_config _ = {
  tile_rule = ();
  map_width = 64;
  map_height = 32;
}

let init_map space map_config rule_config =
  let module Generator = Tiles.Generator.TileInfoBuilder (struct
      let width = map_config.map_width
      let height = map_config.map_height
  end) in
  let open Space in

  (* Initialize tile graph *)
  Generator.init_graph 8;

  (* Initialize rivers *)
  Generator.build_rivers 2;
  Generator.build_features 2;

  let tiles_info = Generator.nodes in
  let tiles = Array.init (Array.length tiles_info) (fun i ->
    let info = tiles_info.(i) in
    let tile_type = info.ttype in
    let quality = Quality.Normal in
    let tile_name = Printf.sprintf "tile_%d" i in
    let tile = Tile.mk_tile tile_name tile_type quality info.cor in
    let rules = rule_config tile_type in
    Array.iter (fun rule -> Environ.install_rule rule tile#get_env) rules;
    space.register_event (Timer.of_int 5) tile;
    tile
  ) in
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = map_config.map_width
    let height = map_config.map_height
  end) in
  let buildings, _ = Array.fold_left (fun (acc,idx) node ->
    let pos = HexCoordinate.from_index idx in
    let siblings = HexCoordinate.radius_siblings1 pos in
    let siblings = List.map (fun pos ->
        HexCoordinate.get_node pos tiles_info
      ) siblings in
    let acc = match Buildings.Generator.generate_building node siblings with
    | None -> acc
    | Some building ->
        assert (node.ttype.features = []);
        acc @ [
        (HexCoordinate.get_node pos tiles :> Object.t)
        , Feature.mk_produce building 1
      ]
    in acc, idx + 1
  ) ([],0) tiles_info in
  tiles, buildings

class elt n = object(self)

  inherit Object.elt n (-1,-1)

  val npcs:(ID.t, Object.t) Hashtbl.t = Hashtbl.create 10
  val buildings:(ID.t, Object.t) Hashtbl.t = Hashtbl.create 10
  val mutable tiles = [||]
  val mutable events: Event.t list = []
  val mutable event_queue = Timer.TriggerQueue.empty
  val mutable update = ObjectSet.empty
  val config = default_config ()

  method space: Object.t Space.t =
    let open Space in
    let module Coordinate = HexCoordinate.Make (struct
      let width = config.map_width
      let height = config.map_height
    end) in
    {
      get_path = (fun _ _ -> [||]);
      the_universe = (fun _ -> (self :> Object.t));
      cancel_event = (fun _ -> ());
      register_event = (fun t o ->
        event_queue <- Timer.TriggerQueue.register_event t o event_queue);
      get_view = (fun cor ->
        Coordinate.sibling_fold cor (fun acc _ sibling _ ->
          sibling :: acc
      ) [] tiles);
      get_tile = (fun cor -> Some (Coordinate.get_node cor tiles));
      set_active = (fun o -> update <- ObjectSet.add o update);
    }

  method get_update: Yojson.Basic.t =
    let objs = ObjectSet.fold (fun v acc ->
      acc @ [v#to_json]
    ) update [] in
    `Assoc [
      ("world", `Assoc [
          ("width", `Int config.map_width);
          ("height", `Int config.map_height);
        ]
      )
      ; ("updates", `List objs)
    ]

  method to_json =
    let seq = List.of_seq @@ Hashtbl.to_seq npcs in
    let npcs = List.fold_left (fun acc (_, v) ->
      acc @ [v#to_json]
    ) [] seq in

    let seq = List.of_seq @@ Hashtbl.to_seq buildings in
    let buildings = List.fold_left (fun acc (_, v) ->
      acc @ [v#to_json]
    ) [] seq in

    `Assoc [
        ("world", `Assoc [
            ("width", `Int config.map_width);
            ("height", `Int config.map_height);
        ])
      ; ("tiles", `List (Array.fold_left (fun acc c->
          acc @ [c#to_json]
        ) [] tiles))
      ; ("npcs", `List (npcs @ buildings))
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
      | Feature.Produce (Attribute.Api.Spawn attr, _)
        when Population.try_spawn () -> begin
          Population.increase_population ();
          let obj = match attr with
          | Attribute.Spawn.Apprentice crk ->
              crk (Event.get_source e).(0)
          | Attribute.Spawn.Creature crk ->
              crk (Event.get_source e).(0)
          | Attribute.Spawn.Mob crk ->
              crk (Event.get_source e).(0)
          | Attribute.Spawn.God crk ->
              crk (Event.get_source e).(0)
          | _ -> assert false
          in
          Hashtbl.add npcs (ID.of_string obj#get_name) (obj:>Object.t);
          let x,y = obj#get_loc in
          let* _ = Lwt_io.printf "spawn at location (%d,%d)\n" x y in
          Lwt.return @@ space.register_event (Timer.of_int 1) (obj:>Object.t)
        end
      | Feature.Hold (Attribute.Api.Object Attribute.Api.Dead, _) ->
        let npc = (Event.get_source e).(0) in
        Lwt.return @@ Hashtbl.remove npcs (ID.of_string npc#get_name)
      | _ -> Lwt.return ()
    ) my_events in

    events <- left_evts @ deliver_events;
    Lwt.return []
  end

  method handle_event space src feature =
    let src = src.(0) in
    match feature with
    | Feature.Produce (Attribute.Api.Spawn attr, _) -> begin
        let building = match attr with
        | Attribute.Spawn.Building crk -> crk src
        | Attribute.Spawn.Apprentice crk -> crk src
        | _ -> assert false
        in
        Hashtbl.add buildings (ID.of_string building#get_name) (building:>Object.t);
        let x,y = building#get_loc in
        let* _ = Lwt_io.printf "building at location (%d,%d)\n" x y in
        space.register_event (Timer.of_int 1) (building:>Object.t);
        Lwt.return []
      end
    | _ -> Lwt.return []

  method handle_command _ = ()

  method init (_:unit) =
    Printexc.record_backtrace true;
    let space = self#space in
    let ts, buildings = init_map space config (TileRule.tile_rule self) in
    tiles <- ts;
    ignore @@ List.iter (fun (s, f) -> ignore @@ self#handle_event space [|s|] f) buildings;
    let center = tiles.(Array.length tiles/2) in
    ignore @@ self#handle_event space [|center|] (Feature.mk_produce (Npc.Generator.generate_player center) 1)

end

let init _ =
  let universe = new elt "Universe" in
  universe#init ();
  universe
