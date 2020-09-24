type village_state = int

let mk_state_info st =
    `Assoc [ ("population", `String (Printf.sprintf "%d" st))]

let make_state _ (* quality *) =
  fun state _ _ (* space self *) -> Common.Api.CommonState.idle_state state

let mk_village tile =
    let name = Core.Name.gen_name "Village" in
    new Api.elt name 100 (make_state Core.Quality.Normal) mk_state_info tile

let init _ = Api.BuildingSpawnSystem.register_spawner "Village" mk_village
