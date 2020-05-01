open Core
open Lwt.Syntax

module Npc = Npc.Api

type coordinate = int * int

type state = {
  name: string;
  features: (Feature.t * (Object.t option)) array;
  last: Timer.time_slice;
}

type tile_state = {
  state: state option;
}


let mk_tile_state desc fs t = {
  name=desc; features=fs; last = t
}

class elt n ds = object (self)

  inherit Object.elt n

  val mutable default_state: (tile_state, string) Object.state_trans = ds

  val mutable state =
    let desc, fs, t = ds { state = None } in
    { state = Some (mk_tile_state desc fs t) }

  method step _  = begin
    let* spawn_events = Lwt.return @@ Environ.apply_rules self#get_env in
    let* step_events = match state.state with
    | Some state' -> begin
        let t = Timer.play state'.last in
        let* fs = if Timer.trigger t then begin
          let (desc, fs, last) = default_state state in
          let fs' = state'.features in
          let* _ = Logger.log "%s finished %s and produce: %s\n" name state'.name
            (Array.fold_left (fun acc (c,_) -> acc ^ " " ^ Feature.to_string c) "" fs') in
          let* _ = Logger.log "[ local_env: %s ]\n" (Environ.dump self#get_env) in
          state <- { state = Some (mk_tile_state desc fs last) };
          Lwt.return @@ Array.to_list fs'
        end else begin
          state <- { state = Some {state' with last = t} };
          Lwt.return @@ []
        end in
        let fs, events = List.fold_left (fun (fs, events) (f, opt_target) ->
          match opt_target with
          | None -> (f::fs), events
          | Some obj -> fs, ((f, obj) :: events);
        ) ([], []) fs in
        self#take_features @@ Array.of_list fs;
        Lwt.return events
      end
    | None -> Lwt.return []
    in Lwt.return (spawn_events @ step_events)
  end
end

type map = {
  tiles: (Object.t option) array;
  width: int;
  height: int;
}

let mk_tile name =
  let tile = Default.tile_type_array.(Random.int 4) in
  let quality = Quality.Normal in
  new elt name (Default.make_state quality tile (Timer.of_int 4))

let mk_map width height: map =
  let map = {
    tiles = Array.make (width * height) None;
    width = width;
    height = height;
  } in
  for i = 0 to (width * height - 1) do
    let tile = mk_tile (Printf.sprintf "tile_%d" i) in
    map.tiles.(i) <- Some tile
  done;
  map

let mk_cor left top = (left,top)

let get_tile (cor:coordinate) map =
  let (top, left) = cor in
  map.tiles.(top * map.width + left)

let step map universe =
   Lwt_list.fold_left_s (fun acc m -> match m with
   | None -> Lwt.return acc
   | Some m ->
       let* fs = m#step universe in
       let* es = (Lwt_list.map_s (fun (f,o) -> Lwt.return (Event.mk_event m o f)) fs) in
       Lwt.return (acc @ es)
   ) [] (Array.to_list map.tiles)

class tile_attr (obj:Object.t) = object
  inherit Attribute.attr
  method name = obj#get_name
  method category = "Tile"
end

let mk_tile_attr t: Attribute.t = (new tile_attr t :> Attribute.t)



