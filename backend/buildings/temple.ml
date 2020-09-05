open Core
open Common
open Api

type temple_state = int

let mk_state_info st =
    `Assoc [ ("level", `String (Printf.sprintf "%d" st)) ]

let make_state _ (* quality *) =
  fun state _ _ (* space self *) -> idle_state state

let mk_temple tile =
    let name = Core.Name.gen_name "Temple" in
    new Api.elt name 1 (make_state Quality.Normal) mk_state_info tile

let init _ = registered_buildings := mk_temple :: !registered_buildings
