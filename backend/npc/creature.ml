open Core
open Common

let _ = Random.self_init ()

type creature_state = {
  health: int;
}

let mk_state_info st =
    `Assoc [
      ("health", `String (Printf.sprintf "%d" st.health))
    ]

let make_state _ = fun state _ _ -> state, Timer.of_int 20


class elt name tile = object (_)

  inherit Api.elt name {health=10} (make_state Quality.Normal)
    mk_state_info tile

  method handle_event _ _ _ = Lwt.return []

  method handle_command _ = ()

end

let mk_creature tile =
    let name = Core.Name.gen_name "Creature" in
    new elt name tile

let creature_rule oref : Object.t Environ.rule =
    let creature = Attribute.Spawn
      (AttributeSpawn.Apprentice mk_creature)
    in
    [| Attribute.WuXing AttributeWuXing.Huo, 5 |]
    , (Feature.mk_produce creature 1, oref)
