module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core
open Common

let _ = Random.self_init ()

let make_state _ = fun state space self ->
  let open Space in
  let around = space.get_view self#get_loc in
  let source = Option.get @@ space.get_tile self#get_loc in
  let  _,_ = around, source in
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
    | _ -> idle_state state

let mk_creature tile =
    let name = Core.Name.gen_name "Creature" in
    new Api.elt name (make_state Quality.Normal) tile

let creature_rule oref : (Feature.t * Object.t) Environ.rule =
    let creature = new AttributeSpawn.ext_attr Creature in
    [| new AttributeWuXing.ext_attr Jing, 3 |]
    , (Feature.mk_produce creature 1, oref)