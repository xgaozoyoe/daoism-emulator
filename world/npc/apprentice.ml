module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeNpc = Core.Attribute.From(Attribute.Npc)

open Core
let apprentice_rule oref : (Feature.t * Object.t) Environ.rule =
    let apprentice = new AttributeNpc.ext_attr Apprentice in
    [| new AttributeWuXing.ext_attr Jing, 2 |]
    , (Feature.mk_produce apprentice 1, oref)

open Quality
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

let make_state quality timeslice = fun _ ->
  let spawn = "Spawn" in
  let features_array =
    Array.map (fun x-> x, None) (make_basic_features (quality_to_total quality)) in
  (spawn, features_array, timeslice)

let mk_apprentice tile = new Api.elt "Apprentice" (make_state Quality.Normal (Timer.of_int 10)) tile
