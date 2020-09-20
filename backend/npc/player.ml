open Core
open Common
let _ = Random.self_init ()

type player_state = {
  health: int;
}

let mk_state_info st =
    `Assoc [
      ("health", `String (Printf.sprintf "%d" st.health))
    ]

let make_state _ = fun state _ _ -> idle_state state

let cmd = `Assoc [
    ("move",`String "coordinate");
    ("attack",`String "coordinate");
    ("train",`String "train");
    ("construct",`String "construct");
    ("think",`String "think");
  ]

class elt name tile = object (self)

  inherit Api.elt ~cmd:cmd name {health=10} (make_state Quality.Normal)
    mk_state_info tile

  method handle_event _ _ _ = Lwt.return []

  method handle_command space command =
    let subcommand = Sdk.Command.subcommand_of_yojson command in
    match subcommand with
    | Ok (Move cor) -> begin
        let src_tile = Option.get @@ space.get_tile (self#get_loc) in
        let target_tile = Option.get @@ space.get_tile cor in
        let mstate, _ = move_state state src_tile target_tile in
        state <- mstate;
        space.cancel_event (self:>Object.t);
        space.register_event (Timer.of_int 1) (self:>Object.t)
      end
    | _ -> raise (Sdk.Command.InvalidCommand
        (Yojson.Safe.to_string command)
      )

end

let mk_player tile =
    let name = Core.Name.gen_name "Player" in
    new elt name tile
