module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core
open Common

let _ = Random.self_init ()

let make_state quality = fun state universe space _ ->
  let pick = Random.int 3 in
  let open Space in
  match pick with
    | 2 -> move_state state
        (Option.get @@ space.pick_from_coordinate (Space.mk_rand_cor state.tile))
        universe
    | 1 -> practise_state quality state
    | 0 -> explore_state state universe
    | _ -> assert false

let mk_apprentice tile =
    let name = Core.Name.gen_name "Apprentice" in
    new Api.elt name (make_state Quality.Normal) tile

let apprentice_rule oref : (Feature.t * Object.t) Environ.rule =
    let apprentice = new AttributeSpawn.ext_attr Apprentice in
    [| new AttributeWuXing.ext_attr Jing, 3 |]
    , (Feature.mk_produce apprentice 1, oref)
