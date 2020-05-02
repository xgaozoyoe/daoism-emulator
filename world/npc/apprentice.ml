module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core
open Attr
open Quality
open Common

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
  { state with
    description = "修炼";
    features = Array.map (fun x-> x, None) (make_basic_features (quality_to_total quality));
    last = Timer.of_int 10;
  }

let explore_state state universe =
  { state with
    description = "探索";
    features = [|Feature.mk_hold (mk_status_attr "stay") 1, Some universe|];
    last = Timer.of_int 5;
  }

let make_state quality = fun (state,s,space) ->
  let pick = Random.int 3 in
  let open Space in
  match pick with
    | 2 -> move_state state
        (Option.get @@ space.pick_from_coordinate (Space.mk_rand_cor ()))
    | 1 -> practise_state quality state
    | 0 -> explore_state state s
    | _ -> assert false

let mk_apprentice tile =
    let name = Core.Name.gen_name "Apprentice" in
    new Api.elt name (make_state Quality.Normal) tile
