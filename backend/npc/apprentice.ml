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

  inherit Api.elt name {health=2} (make_state Quality.Normal)
    mk_state_info tile

  method handle_event space src feature = begin
    let open Space in
    match feature with
    | Feature.Produce (Damage _, dmg) ->
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
        let events = Array.map (fun (f, t) ->
          (Event.mk_event f (self:>Object.t) (Option.get t))
        ) s.deliver in
        let drop = Array.map (fun f ->
          (Event.mk_event f (self:>Object.t) loc_tile)
        )  (Inventory.get_drop (self#get_inventory)) in
        Lwt.return @@ (Array.to_list events) @ (Array.to_list drop)
      end else
        Lwt.return []
    | Hold (Attribute.Api.Notice Meet, _)  ->
      Lwt.return []
    | _ -> Lwt.return []
  end

  method handle_command _ _ = ()

end

let mk_apprentice tile =
  let name = Core.Name.gen_name "Apprentice" in
  let obj = new elt name tile in
  Object.add_to_inventory obj 0 (Attribute.Api.Equipment (Attribute.Equipment.mk_armor 1), 1);
  Object.add_to_inventory obj 1 (Attribute.Api.Equipment (Attribute.Equipment.mk_armor 1), 1);
  Object.add_to_inventory obj 2 (Attribute.Api.Equipment (Attribute.Equipment.mk_armor 1), 1);
  Object.add_to_inventory obj 3 (Attribute.Api.Equipment (Attribute.Equipment.mk_armor 1), 1);
  obj

let apprentice_rule oref : Object.t Environ.rule =
    let apprentice = Attribute.Api.Spawn
      (Attribute.Spawn.Apprentice mk_apprentice)
    in
    [| Attribute.Api.WuXing Attribute.WuXing.Jing, 3 |]
    , (Feature.mk_produce apprentice 1, oref)
