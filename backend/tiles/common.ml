module AttributeWuXing = Attribute.WuXing
module AttributeBase = Attribute.Base
module AttributeSpawn = Attribute.Spawn
module Attribute = Attribute.Api

open Core
type tile_state = {
  name: string;
  deliver: (Object.t Feature.t * Object.t option) array;
}

let mk_state (desc, es) = {
  name = desc;
  deliver = es;
}

let tile_state_to_json ns: Yojson.Basic.t =
  `Assoc [
    ("name", `String ns.name)
    ; ("deliver", `List (List.map (fun pre -> Object.pre_event_to_json pre)
        (Array.to_list ns.deliver)))
   ]


