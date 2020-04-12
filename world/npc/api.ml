open Core
open Timer

module Make (O:Object.Interface) = struct

  type npc_state = {
    name: string;
    features: O.Env.Feature.t list;
    tile: O.t ref;
  }

  type t = {
    body: O.t;
    state: npc_state * time_slice;
    defaut_state: unit -> string * O.Env.Feature.t list * time_slice;
  }

  let mk_npc_state desc fs tile = {name=desc; features=fs; tile=tile}

  let mk_npc obj defaut tile =
    let (desc, fs, t) = defaut () in
    { body = obj; state = (mk_npc_state desc fs tile), t; defaut_state = defaut }

  let one_step npc =
    let (state, t), fs = match npc.state with state, t -> begin
      let t = Timer.play t in
      if trigger t then begin
        Logger.log "%s finished %s\n" (O.to_string npc.body) state.name;
        let (desc, fs, last) = npc.defaut_state () in
        let tile = state.tile in
        (mk_npc_state desc fs tile, last), (fst npc.state).features
      end else
        (state, t), []
    end in
    { npc with body = O.take_features fs npc.body; state = (state, t) }
end
