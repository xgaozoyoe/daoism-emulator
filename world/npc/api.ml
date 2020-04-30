open Core
open Lwt.Syntax

type dfs = (Feature.t * (Object.t option)) array

type npc_state = {
  name: string;
  features: (Feature.t * (Object.t option)) array;
  last: Timer.time_slice;
  tile: Object.t; (* position of the npc *)
}

type t = {
  state: npc_state option;
}

let mk_npc_state desc (fs:dfs) tile t = { name=desc; features=fs; tile=tile; last = t }

class elt n ds (tile:Object.t) = object (self)

  inherit Object.elt n

  val mutable default_state : (t, string) Object.state_trans = ds

  val mutable state =
    let desc, fs, t = ds { state = None } in
    { state = Some (mk_npc_state desc fs tile t) }

  method step _ = begin
    match state.state with
    | Some state' -> begin
        let t = Timer.play state'.last in
        let* fs = if Timer.trigger t then begin
          let* _ = Logger.log "%s finished %s\n" name state'.name in
          let (desc, fs, last) = default_state state in
          let tile' = state'.tile in
          let fs' = state'.features in
          state <- { state = Some (mk_npc_state desc fs tile' last) };
          Lwt.return fs'
        end else begin
          state <- { state = Some {state' with last = t} };
          let* _ = Logger.log "%s tick %s, remain: %s\n" name state'.name (Timer.to_string t) in
          Lwt.return [||]
        end in
        let fs, events = Array.fold_left (fun (fs, events) (f, opt_target) ->
          match opt_target with
          | None -> (f::fs), events
          | Some obj -> fs, ((f, obj) :: events);
        ) ([], []) fs in
        self#take_features @@ Array.of_list fs;
        Lwt.return events
      end
    | None -> Lwt.return []
  end
end
