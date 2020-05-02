module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core
open Attr

type npc_state = {
  description: string;
  features: (Feature.t * (Object.t option)) array;
  last: Timer.time_slice;
  tile: Object.t; (* position of the npc *)
}

let npc_state_to_json ns =
  `Assoc [
    ("description", `String ns.description)
    ; ("features", `List (List.map (fun pre -> Object.pre_event_to_json pre)
        (Array.to_list ns.features)))
    ; ("last", Timer.to_json ns.last)
    ; ("tile", `String ns.tile#get_name)
   ]

let apprentice_rule oref : (Feature.t * Object.t) Environ.rule =
    let apprentice = new AttributeSpawn.ext_attr Apprentice in
    [| new AttributeWuXing.ext_attr Jing, 3 |]
    , (Feature.mk_produce apprentice 1, oref)

let move_state state tile =
  {
    description = "移动";
    features = if state.tile#get_name = tile#get_name
      then [||] else
       [|Feature.mk_hold (mk_status_attr "leave") 1, Some state.tile
       ; Feature.mk_hold (mk_status_attr "enter") 1, Some tile
       |];
    last = Timer.of_int 5;
    tile = tile
  }

let dead_state tile universe = {
    features=[|
        Feature.mk_hold (mk_status_attr "leave") 1, Some tile
      ; Feature.mk_hold (mk_status_attr "dead") 1, Some universe
    |];
    last=Timer.of_int 1; tile=tile; description="死亡"
  }





