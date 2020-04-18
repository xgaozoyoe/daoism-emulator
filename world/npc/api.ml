open Core
open Timer

module Make (O:Object.Interface) = struct

  type npc_state = {
    name: string;
    features: (O.Env.Feature.t * (O.t ref option)) list;
    last: time_slice;
    tile: O.t ref; (* position of the npc *)
  }

  type t = {
    state: npc_state option;
  }

  let mk_npc_state desc fs tile t = { name=desc; features=fs; tile=tile; last = t }

  class elt n ds tile = object (self)

    inherit O.elt n

    val mutable defaut_state : (t, string) O.state_trans = ds

    val mutable state =
      let desc, fs, t = ds { state = None } in
      { state = Some (mk_npc_state desc fs tile t) }

    method step _ = begin
      match state.state with
      | Some state' -> begin
          let t = Timer.play state'.last in
          let fs = if trigger t then begin
            Logger.log "%s finished %s\n" name state'.name;
            let (desc, fs, last) = defaut_state state in
            let tile' = state'.tile in
            let fs' = state'.features in
            state <- { state = Some (mk_npc_state desc fs tile' last) };
            fs'
          end else begin
            state <- { state = Some {state' with last = t} };
            []
          end in
          let fs, events = List.fold_left (fun (fs, events) (f, opt_target) ->
            match opt_target with
            | None -> (f::fs), events
            | Some obj_ref -> fs, ((f, obj_ref) :: events);
          ) ([], []) fs in
          self#take_features fs;
          events
        end
      | None -> []
    end
  end
end
