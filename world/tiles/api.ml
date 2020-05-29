open Core
open Space
open Lwt.Syntax
open Common

module NpcAttr = Npc.Attr
module Npc = Npc.Api

class elt n ttype ds = object (self)

  inherit Object.elt n

  val mutable state_trans: tile_state Object.state_trans = ds

  val mutable state = {deliver=[||]; name="初"}

  val mutable holds: Object.t list = []

  val mutable tile_type = ttype

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("ttype", Default.to_json tile_type)
    ; ("state", tile_state_to_json state)
    ; ("env", Environ.to_json self#get_env)
  ]

  method handle_event _ src feature = begin
    let src = src.(0) in
    match feature with
    | Hold (attr, _) when attr#test "Status" -> begin
        let _ = match attr#name with
        | "enter" -> holds <- src :: holds
        | "leave" -> holds <- List.filter_map (fun c ->
            if (c#get_name = src#get_name) then None else Some c
          ) holds
        | _ -> ()
        in
        Lwt.return []
      end
    | _ -> Lwt.return []

    (*
    let adjacent_events = List.fold_left (fun acc t ->
      let others = List.filter_map (fun c ->
            if (c#get_name = t#get_name) then None else Some c
      ) holds in
      match others with
      | [] -> acc
      | _ -> acc @ [(Feature.mk_hold (NpcAttr.mk_tile_attr ()) 1, Array.of_list others, t)]
    ) [] holds in
    *)
  end

  (* step universe space -> event list *)
  method step space = begin

    let* step_events = begin
      let fs, events = Array.fold_left (fun (fs, events) (f, opt_target) ->
        match opt_target with
        | None -> (f::fs), events
        | Some obj -> fs, ((f, [|(self:>Object.t)|], obj) :: events);
      ) ([], []) state.deliver in
      self#take_features @@ Array.of_list fs;
      Lwt.return events
    end in

    let* spawn_events = Lwt.return @@ List.map (fun (f,o) ->
        (f, [|(self:>Object.t)|], o)
      ) (Environ.apply_rules self#get_env) in

    (*
    let* _ = Logger.log "[ %s <%s> local_env: %s ]\n" (self#get_name) state.name (Environ.dump self#get_env) in
    let* _ = Logger.log "[ %s <%s> holds:%s ]\n" (self#get_name) state.name
         (List.fold_left (fun acc c -> acc ^ " " ^ c#get_name) "" holds) in
    *)

    (* Register step action after 5 timeslice *)
    let* s, ts = Lwt.return @@ state_trans state space (self:>Object.t) in
    state <- s;
    space.register_event ts (self:>Object.t);
    Lwt.return (spawn_events @ step_events)
  end
end

type map = {
  tiles: (Object.t option) array;
  width: int;
  height: int;
  mutable path: Object.t array -> Object.t array -> Object.t array
}

let mk_tile name typ quality =
  let tile = new elt name typ (
    Default.make_default_state quality typ (Timer.of_int 4)
  ) in
  Default.set_default_bound quality typ 10 tile;
  tile

let get_tile (cor:coordinate) map =
  let (top, left) = cor in
  map.tiles.(top * map.width + left)

let mk_map width height: map = {
    tiles = Array.make (width * height) None;
    width = width;
    height = height;
    path = fun _ _ -> [||]
  }

let init_map space map rule_config =
  Printexc.record_backtrace true;
  let module Generator = Generator.TileInfoBuilder (struct
      let width=map.width
      let height=map.height
   end) in

  (* Initialize tile graph *)
  Generator.init_graph 2;

  (* Initialize rivers *)
  Generator.build_rivers 2;

  let tiles_info = Generator.nodes in
  for i = 0 to (map.width * map.height - 1) do
    let tile_type = tiles_info.(i).ttype in
    let quality = Quality.Normal in
    let tile = mk_tile (Printf.sprintf "tile_%d" i) tile_type quality in
    let rules = rule_config tile_type in
    Array.iter (fun rule -> Environ.install_rule rule tile#get_env) rules;
    map.tiles.(i) <- Some tile;
    space.register_event (Timer.of_int 5) tile;
  done;
  map

let to_json map =
  `Assoc [
    ("width", `Int map.width);
    ("height", `Int map.height);
    ("tiles",
       `List (Array.fold_left (fun acc c->
          acc @ [(Option.get c)#to_json]
        ) [] map.tiles)
    )
  ]
