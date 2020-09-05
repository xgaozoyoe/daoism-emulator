module AttributeWuXing = Attribute.WuXing
module AttributeBase = Attribute.Base
module AttributeSpawn = Attribute.Spawn
module AttributeDamage = Attribute.Damage
module Attribute = Attribute.Api

open Core
open Attr
open Quality

type 'a npc_state = {
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

let quality_to_total = function
| Unique -> 200
| Rare -> 150
| Elite -> 100
| Normal -> 80
| Antique -> Random.int 200

let make_basic_features total =
  let open Core in
  let ratio = [|Random.int 10; Random.int 10; Random.int 10; Random.int 10; Random.int 10|] in
  let sum = Array.fold_left (fun acc c -> c + acc) 0 ratio in
  let ratio = Array.map (fun c -> (c * total) / sum) ratio in
  let wx = [|
      Attribute.WuXing Jing;
      Attribute.WuXing Mu;
      Attribute.WuXing Shui;
      Attribute.WuXing Huo;
      Attribute.WuXing Tu;
  |] in
  Array.map2 (fun amount wx -> Feature.mk_produce wx amount) ratio wx

let practise_state quality state =
  let es = Array.map (fun x-> x, None) (make_basic_features (quality_to_total quality)) in
  { state with deliver = es; description = "修炼" }, Timer.of_int 9

let move_state state src target =
  let es = begin
    if src = target then [||]
    else [|
      Feature.mk_hold (mk_obj_attr Attribute.Leave) 1, Some src
      ; Feature.mk_hold (mk_obj_attr Attribute.Enter) 1, Some target
    |]
  end in
  { state with deliver = es; description = "移动"}, Timer.of_int 4

let explore_state state universe =
  let es = [|Feature.mk_hold (mk_obj_attr Attribute.Stay) 1, Some universe|] in
  { state with deliver = es; description = "探索"}, Timer.of_int 5

let idle_state state =
  { state with deliver = [||]; description = "休息"}, Timer.of_int 5

let dead_state state tile universe =
  let es = [|
    Feature.mk_hold (mk_obj_attr Attribute.Leave) 1, Some tile
    ; Feature.mk_hold (mk_obj_attr Attribute.Dead) 1, Some universe
  |] in
  {state with deliver = es; description="死亡"}, Timer.of_int 1

