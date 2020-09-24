open Core

type temple_state = int

let mk_state_info st =
    `Assoc [ ("level", `String (Printf.sprintf "%d" st)) ]

let make_state _ (* quality *) =
  fun state _ _ (* space self *) -> Common.Api.CommonState.idle_state state

let mk_temple tile =
    let name = Core.Name.gen_name "Temple" in
    new Api.elt name 1 (make_state Quality.Normal) mk_state_info tile

let init _ = Api.BuildingSpawnSystem.register_spawner "Temple" mk_temple
