open Core
open Space
open Lwt.Syntax

module Npc = Npc.Api

type tile_state = {
  name: string;
  features: (Feature.t * (Object.t option)) array;
  last: Timer.time_slice;
}

let tile_state_to_json ns: Yojson.Basic.t=
  `Assoc [
    ("description", `String ns.name)
    ; ("features", `List (List.map (fun pre -> Object.pre_event_to_json pre)
        (Array.to_list ns.features)))
    ; ("last", Timer.to_json ns.last)
   ]

let mk_state (desc, f, t) = {
    name = desc;
    features = f;
    last = t;
  }

class elt n ds = object (self)

  inherit Object.elt n

  val mutable state_trans: tile_state Object.state_trans = ds

  val mutable state = {features=[||]; last=Timer.of_int 1; name="初"}

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("state", tile_state_to_json state)
    ; ("env", Environ.to_json self#get_env)
  ]

  method step universe space = begin
    let* spawn_events = Lwt.return @@ Environ.apply_rules self#get_env in
    let* step_events = begin
      let t = Timer.play state.last in
      let* fs = if Timer.trigger t then begin
        let fs' = state.features in
        let ns = state_trans (state,universe,space) in
        state <- ns;
        Lwt.return @@ Array.to_list fs'
      end else begin
        state <- {state with last = t};
        Lwt.return @@ []
      end in
      let fs, events = List.fold_left (fun (fs, events) (f, opt_target) ->
        match opt_target with
        | None -> (f::fs), events
        | Some obj -> fs, ((f, obj) :: events);
      ) ([], []) fs in
      self#take_features @@ Array.of_list fs;
      Lwt.return events
    end in
    let* _ = Logger.log "[ %s <%s> local_env: %s ]\n" (self#get_name) state.name (Environ.dump self#get_env) in
    Lwt.return (spawn_events @ step_events)
  end
end

type map = {
  tiles: (Object.t option) array;
  width: int;
  height: int;
}

let mk_tile name typ quality =
  new elt name (fun s -> mk_state @@ Default.make_default_state quality typ (Timer.of_int 4) s)

let mk_map width height: map =
  let map = {
    tiles = Array.make (width * height) None;
    width = width;
    height = height;
  } in map

let init_map map rule_config =
  for i = 0 to (map.width * map.height - 1) do
    let tile_type = Default.tile_type_array.(Random.int 4) in
    let quality = Quality.Normal in
    let tile = mk_tile (Printf.sprintf "tile_%d" i) tile_type quality in
    let rules = rule_config tile_type in
    Array.iter (fun rule -> Environ.install_rule rule tile#get_env) rules;
    map.tiles.(i) <- Some tile
  done;
  map

let get_tile (cor:coordinate) map =
  let (top, left) = cor in
  map.tiles.(top * map.width + left)

let step map universe space =
   let* _ = Lwt_io.printf ("step\n") in
   Lwt_list.fold_left_s (fun acc m -> match m with
   | None -> Lwt.return acc
   | Some m ->
       let* fs = m#step universe space in
       let* es = (Lwt_list.map_s (fun (f,o) -> Lwt.return (Event.mk_event m o f)) fs) in
       Lwt.return (acc @ es)
   ) [] (Array.to_list map.tiles)

let to_json map =
  `Assoc [
    ("width", `Int map.width);
    ("height", `Int map.height);
    ("Tiles",
       `List (Array.fold_left (fun acc c->
          acc @ [(Option.get c)#to_json]
        ) [] map.tiles)
    )
  ]
