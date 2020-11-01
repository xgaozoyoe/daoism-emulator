open Lwt.Syntax
module Apprentice = Npc.Apprentice
module Creature = Npc.Creature
module Tile = Tiles.Api
open Utils
open Core
open Sdk.UID

module ObjectSet = Set.Make (Object)

let section = Lwt_log.Section.make "universe"

type config = {
  tile_rule: unit;
  map_width: int;
  map_height: int;
}

let default_config _ = {
  tile_rule = ();
  map_width = 6;
  map_height = 4;
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
    let tile_name = Printf.sprintf "Tile.%d" i in
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

  val npcs:(UID.t, Object.t) Hashtbl.t = Hashtbl.create 10
  val buildings:(UID.t, Object.t) Hashtbl.t = Hashtbl.create 10
  val mutable tiles = [||]
  val mutable events: Object.t Event.t list = []
  val event_queue = ref (Timer.TriggerQueue.mk_head Timer.TriggerQueue.empty)
  val mutable update = ObjectSet.empty
  val config = default_config ()

  method get_events = event_queue

  method space: Object.t Space.t =
    let open Space in
    let module Coordinate = HexCoordinate.Make (struct
      let width = config.map_width
      let height = config.map_height
    end) in
    {
      get_path = (fun _ _ -> [||]);
      the_universe = (fun _ -> (self :> Object.t));
      cancel_event = (fun o ->
        let e = o#get_event_entry in
        Timer.TriggerQueue.cancel_event e;
        o#set_event_entry Timer.TriggerQueue.empty
      );
      register_event = (fun t o ->
        o#set_event_entry @@
            Timer.TriggerQueue.register_event t o event_queue
      );
      get_view = (fun cor -> List.map (fun cor ->
        Coordinate.get_node cor tiles
      ) (Array.to_list (Coordinate.valid_siblings cor)));
      get_tile = (fun cor -> Some (Coordinate.get_node cor tiles));
      set_active = (fun o -> update <- ObjectSet.add o update);
      get_npc = (fun id -> Hashtbl.find npcs id);
      get_tile_by_id = (fun uid -> begin
        let idx = int_of_string (UID.get_middle_name uid) in
        Some (Coordinate.get_node (Coordinate.from_index idx) tiles)
      end);
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

  method handle_event space src feature =
    match feature with
    | Feature.Produce (Attribute.Api.Spawn attr, _) -> begin
        let obj = match attr with
        | Attribute.Spawn.Apprentice crk
          when Population.try_spawn () -> begin
            Population.increase_population ();
            Some (crk src)
          end
        | Attribute.Spawn.Creature crk -> Some (crk src)
        | Attribute.Spawn.Mob crk -> Some (crk src)
        | Attribute.Spawn.God crk -> Some (crk src)
        | Attribute.Spawn.Building crk -> Some (crk src)
        | _ -> None
        in
        match obj with
        | Some obj -> begin
            Hashtbl.add npcs (UID.of_string obj#get_name) (obj:>Object.t);
            let x,y = obj#get_loc in
            let* _ = Lwt_io.printf "%s spawn at location (%d,%d)\n" obj#get_name x y in
            space.register_event (Timer.of_int 1) (obj:>Object.t);
            Lwt.return []
          end
        | _ -> Lwt.return []
      end
    | Feature.Hold (Attribute.Api.Notice Attribute.Api.Dead, _) ->
      let npc = src in
      self#space.cancel_event npc;
      Hashtbl.remove npcs (UID.of_string npc#get_name);
      Lwt.return []
    | _ -> Lwt.return []

  method fetch_and_handle_events space = begin
    let seq = Timer.TriggerQueue.fetch_events event_queue true in
    let* _ = Timer.TriggerQueue.dump !event_queue (fun x -> x#get_name) in

    let* evts = Lwt_list.fold_left_s (fun acc v ->
      let* es = v#step space in
      Lwt.return @@ es @ acc
    ) [] seq in
    Lwt.return evts
  end


  method step space : Object.t Event.t list Lwt.t= begin
    update <- ObjectSet.empty;
    let* _ = Lwt_io.printf ("step...\n") in
    let rec aux (evts:Object.t Event.t list) = begin
      let* left_evts = Lwt_list.fold_left_s (fun acc e ->
        let target = Event.get_target e in
        let* _ = Lwt_log.debug_f ~section "[ set active: %s ]" self#get_name in
        self#space.set_active target;
        let* evts = target#handle_event (self#space) (Event.get_source e) (Event.get_feature e) in
        Lwt.return @@ acc @ evts
      ) [] evts in

      (* uncomment this to debug event_queue
       * let* _ = Timer.TriggerQueue.dump event_queue (fun x -> x#get_name) in
       *)
      let* _ = Timer.TriggerQueue.dump !event_queue (fun x -> x#get_name) in
      let* evts = self#fetch_and_handle_events space in

      let my_events, deliver_events =
        List.partition (fun e -> (Event.get_target e)#get_name = self#get_name)
        (left_evts @ evts)
      in
(*
      let* _ = Lwt_list.iter_s (fun e -> Lwt_log.debug_f ~section "[ universe event: %s ]\n"
          (Event.log_string (fun x->x#get_name) e)) my_events in
      let* _ = Lwt_list.iter_s (fun e -> Lwt_log.debug_f ~section "[ deliver event: %s ]\n"
          (Event.log_string (fun x->x#get_name) e)) deliver_events in
*)
      (* Handle events that are generated for universe *)
      let* _ = Lwt_list.iter_s (fun e ->
        let feature = Event.get_feature e in
        let src = Event.get_source e in
        let* _ = self#handle_event space src feature in
        Lwt.return_unit
      ) my_events in

      match (deliver_events) with
      | [] -> Lwt.return []
      | evts -> aux evts
    end in
    aux []
  end

  method handle_command _ _ = ()

  method deliver_command space target_id cmd =
    let c = Hashtbl.find npcs (UID.of_string target_id) in
    self#space.set_active c;
    c#handle_command space cmd

  method init (_:unit) =
    Printexc.record_backtrace true;
    let space = self#space in
    let ts, buildings = init_map space config (TileRule.tile_rule self) in
    tiles <- ts;
    ignore @@ List.iter (fun (s, f) -> ignore @@ self#handle_event space s f) buildings;
    let center = tiles.(Array.length tiles/2) in
    ignore @@ self#handle_event space center (Feature.mk_produce (Npc.Generator.generate_player center) 1)

end

let init _ =
  let universe = new elt "Universe" in
  universe#init ();
  universe
