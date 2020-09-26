open Lwt.Syntax
open Core
open Common.Api

let _ = Random.self_init ()

type apprentice_state = {
  health: int;
}

let mk_state_info st =
    `Assoc [
      ("health", `String (Printf.sprintf "%d" st.health))
    ]

let make_state quality = fun state space self ->
  let open Space in
  let around = space.get_view self#get_loc in
  let source = Option.get @@ space.get_tile self#get_loc in
  let pick = Random.int 3 in
  match pick with
    | 2 -> begin
        let len = List.length around in
        let target = if len > 0 then
          List.nth around (Random.int len)
        else
          source
        in
        CommonState.move_state state source target
      end
    | 1 -> CommonState.practise_state quality state
    | 0 -> CommonState.explore_state state (space.the_universe ())
    | _ -> CommonState.practise_state quality state

class elt name tile = object (self)

  inherit Api.elt name {health=10} (make_state Quality.Normal)
    mk_state_info tile

  method handle_event space src feature = begin
    let open Space in
    match feature with
    | Feature.Produce (Damage _, dmg) ->
      let src = src.(0) in
      let* _ = Lwt_io.printf "%s 受到了来自 %s 的 %d 点攻击\n"
        self#get_name src#get_name dmg in
      let inner_state = state.state in
      let health = inner_state.health - dmg in
      let inner_state = {health = health} in
      state <- {state with state = inner_state};
      if health <= 0 then begin
        let loc_tile = Option.get @@ space.get_tile (self#get_loc) in
        let s, _ = CommonState.dead_state state loc_tile (space.the_universe ()) in
        state <- s;
        (* space register self t *)
        Lwt.return []
      end else
        Lwt.return []
    | Hold (Attribute.Api.Object Meet, _)  ->
      Lwt.return [Feature.mk_produce
        (* FIXME: src might be empty ? *)
        (Attribute.Api.Damage Attribute.Damage.Straight) 1, [|(self :> Object.t)|], src.(0)]
    | _ -> Lwt.return []
  end

  method handle_command _ _ = ()

end

let mk_apprentice tile =
  let name = Core.Name.gen_name "Apprentice" in
  let obj = new elt name tile in
  obj

let apprentice_rule oref : Object.t Environ.rule =
    let apprentice = Attribute.Api.Spawn
      (Attribute.Spawn.Apprentice mk_apprentice)
    in
    [| Attribute.Api.WuXing Attribute.WuXing.Jing, 3 |]
    , (Feature.mk_produce apprentice 1, oref)
