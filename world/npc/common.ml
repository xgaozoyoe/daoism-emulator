module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core
open Attr
open Quality

type npc_state = {
  description: string;
  tile: Object.t; (* position of the npc *)
  deliver: (Feature.t * Object.t option) array;
  health: int;
}

let npc_state_to_json ns =
  `Assoc [
    ("description", `String ns.description)
    ; ("tile", `String ns.tile#get_name)
    ; ("health", `Int ns.health)
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
  let open AttributeWuXing in
  let ratio = [|Random.int 10; Random.int 10; Random.int 10; Random.int 10; Random.int 10|] in
  let sum = Array.fold_left (fun acc c -> c + acc) 0 ratio in
  let ratio = Array.map (fun c -> (c * total) / sum) ratio in
  let wx = [|
      new ext_attr Jing;
      new ext_attr Mu;
      new ext_attr Shui;
      new ext_attr Huo;
      new ext_attr Tu;
  |] in
  Array.map2 (fun amount wx -> Feature.mk_produce wx amount) ratio wx

let practise_state quality state =
  let es = Array.map (fun x-> x, None) (make_basic_features (quality_to_total quality)) in
  { state with deliver = es; description = "修炼" }, Timer.of_int 9

let move_state state tile _ =
  let es = if state.tile#get_name = tile#get_name then [||]
  else [|
    Feature.mk_hold (mk_status_attr "leave") 1, Some state.tile
    ; Feature.mk_hold (mk_status_attr "enter") 1, Some tile
  |] in
  { state with deliver = es; description = "移动"}, Timer.of_int 4

let explore_state state universe =
  let es = [|Feature.mk_hold (mk_status_attr "stay") 1, Some universe|] in
  { state with deliver = es; description = "探索"}, Timer.of_int 5

let dead_state state universe =
  let es = [|
    Feature.mk_hold (mk_status_attr "leave") 1, Some state.tile
    ; Feature.mk_hold (mk_status_attr "dead") 1, Some universe
  |] in
  {state with deliver = es; description="死亡"}, Timer.of_int 1
