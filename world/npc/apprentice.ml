module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core
open Common

let _ = Random.self_init ()

let make_state quality = fun state space self ->
  let open Space in
  let around = space.get_view state.coordinate in
  let source = space.tile self#loc
  match pick with
    | 2 -> move_state state source
        (space.get_tile around.(Random.int (List.length around + 1)))
        (space.the_universe ())
    | 1 -> practise_state quality state
    | 0 -> explore_state state (space.the_universe ())
    | _ -> practise_state quality state

let mk_apprentice tile =
    let name = Core.Name.gen_name "Apprentice" in
    new Api.elt name (make_state Quality.Normal) tile

let apprentice_rule oref : (Feature.t * Object.t) Environ.rule =
    let apprentice = new AttributeSpawn.ext_attr Apprentice in
    [| new AttributeWuXing.ext_attr Jing, 3 |]
    , (Feature.mk_produce apprentice 1, oref)
