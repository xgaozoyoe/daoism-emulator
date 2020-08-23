open Core
open Common
let _ = Random.self_init ()

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
        move_state state source target
      end
    | 1 -> practise_state quality state
    | 0 -> explore_state state (space.the_universe ())
    | _ -> practise_state quality state

let mk_apprentice tile =
    let name = Core.Name.gen_name "Apprentice" in
    new Api.elt name (make_state Quality.Normal) tile

let apprentice_rule oref : Object.t Environ.rule =
    let apprentice = Attribute.Spawn
      (AttributeSpawn.Apprentice mk_apprentice)
    in
    [| Attribute.WuXing AttributeWuXing.Jing, 3 |]
    , (Feature.mk_produce apprentice 1, oref)
