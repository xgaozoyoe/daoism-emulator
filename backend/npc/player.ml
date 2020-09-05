open Core
let _ = Random.self_init ()

type player_state = {
  health: int;
}

let mk_state_info st =
    `Assoc [
      ("health", `String (Printf.sprintf "%d" st.health))
    ]

let make_state _ = fun state _ _ -> state, Timer.of_int 10

class elt name tile = object (_)

  inherit Api.elt name {health=10} (make_state Quality.Normal)
    mk_state_info tile

  method handle_event _ _ _ = Lwt.return []

end

let mk_player tile =
    let name = Core.Name.gen_name "Player" in
    new elt name tile
