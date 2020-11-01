open Core
open Sdk.UID

let _ = Random.self_init ()

type player_state = {
  health: int;
}

let mk_state_info st =
    `Assoc [
      ("health", `String (Printf.sprintf "%d" st.health))
    ]

let make_state _ = fun state _ _ -> Common.Api.CommonState.idle_state state

let cmd = `Assoc [
    ("Move",`String "coordinate");
    ("Attack",`String "target");
    ("Train",`String "id");
    ("Construct",`String "id");
    ("Think",`String "unit");
  ]

class elt name tile = object (self)

  inherit Api.elt ~cmd:cmd name {health=10} (make_state Quality.Normal)
    mk_state_info tile

  method handle_event _ _ _ = Lwt.return []

  method handle_command space command =
    let subcommand = Sdk.Command.subcommand_of_yojson command in
    match subcommand with
    | Ok (Move tile_id) -> begin
        let tile_id = UID.of_string tile_id in
        let src_tile = Option.get @@ space.get_tile (self#get_loc) in
        let target_tile = Option.get @@ space.get_tile_by_id tile_id in
        let mstate, _ = Common.Api.CommonState.move_state state src_tile target_tile in
        state <- mstate;
        space.cancel_event (self:>Object.t);
        space.register_event (Timer.of_int 1) (self:>Object.t)
      end
    | Ok (Attack id) -> begin
        try
          (* Might Raise Not_found *)
          let target = space.get_npc (UID.of_string id) in
          let mstate, _ = Common.Api.CommonState.fight_state state target in
          state <- mstate;
          space.cancel_event (self:>Object.t);
          space.register_event (Timer.of_int 0) (self:>Object.t)
        with _ ->
          raise (Sdk.Command.InvalidCommand (id ^ " Not_found"))
      end
    | _ -> raise (Sdk.Command.InvalidCommand
        (Yojson.Safe.to_string command)
      )

end

let mk_player tile =
    let open Common in
    let name = Core.Name.gen_name "Player" in
    let obj = new elt name tile in
    let basic_features = Level.BasicSystem.make_features 150 in
    let wuxing_features = Level.WuXingSystem.make_features 150 in
    obj#take_features basic_features;
    obj#take_features wuxing_features;
    obj
