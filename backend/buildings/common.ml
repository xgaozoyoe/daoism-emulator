module AttributeWuXing = Attribute.WuXing
module AttributeBase = Attribute.Base
module AttributeSpawn = Attribute.Spawn
module Attribute = Attribute.Api

open Core

type 'a building_state = {
  description: string;
  deliver: ((Object.t Feature.t) * Object.t option) array;
  state: 'a;
}

let state_to_json ns info_builder =
  `Assoc [
    ("description", `String ns.description)
    ; ("extra", info_builder ns.state)
    ; ("features", `List (List.map (fun pre -> Object.pre_event_to_json pre)
        (Array.to_list ns.deliver)))
   ]

let idle_state state =
  { state with deliver = [||]; description = "空闲"}, Timer.of_int 5
