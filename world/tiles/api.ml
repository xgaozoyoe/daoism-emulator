open Core

module Apprentice = Npc.Apprentice
module Npc = Npc.Api

type coordinate = int * int

type state = {
  name: string;
  features: (Feature.t * (Object.t ref option)) array;
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

  val mutable defaut_state: (tile_state, string) Object.state_trans = ds

  val mutable state =
    let desc, fs, t = ds { state = None } in
    { state = Some (mk_tile_state desc fs t) }

  method step universe : (Feature.t * Object.t ref) list = begin
    let spawn_events = List.map (fun f -> (f, universe)) (Environ.apply_rules self#get_env) in
    (*
    O.Env.fold (fun key (attr, amount) acc ->
      let npc_attr = new Universe.Api.attrNPC Universe.Api.Apprentice in
      (Feature.mk_produce npc_attr 1, Some universe)
    ) () self#get_env
    *)
    let step_events = match state.state with
    | Some state' -> begin
        let t = Timer.play state'.last in
        let fs = if Timer.trigger t then begin
          Logger.log "%s finished %s\n" name state'.name;
          let (desc, fs, last) = defaut_state state in
          let fs' = state'.features in
          state <- { state = Some (mk_tile_state desc fs last) };
          Array.to_list fs'
        end else begin
          Logger.log "%s tick %s, remain: %s\n" name state'.name (Timer.to_string t);
          state <- { state = Some {state' with last = t} };
          []
        end in
        let fs, events = List.fold_left (fun (fs, events) (f, opt_target) ->
          match opt_target with
          | None -> (f::fs), events
          | Some obj_ref -> fs, ((f, obj_ref) :: events);
        ) ([], []) fs in
        self#take_features @@ Array.of_list fs;
        events
      end
    | None -> []
    in spawn_events @ step_events
  end
end

type tile = Object.t

type map = (tile option) array

let mk_tile name : tile =
  let dfst = Default.make_state Quality.Normal 200 in
  new elt name (Default.make_state dfst (Timer.of_int 10))

let mk_map width height : map =
  let map = Array.make (width * height) None in
  for i = 0 to (width * height - 1) do
    map.(i) <- Some (mk_tile (Printf.sprintf "t%d" i))
  done;
  map

let step map universe =
   Array.fold_left (fun acc m -> match m with
   | None -> acc
   | Some m -> acc @ m#step universe
   ) [] map
