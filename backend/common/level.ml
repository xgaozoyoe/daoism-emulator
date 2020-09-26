module AttributeWuXing = Attribute.WuXing
module AttributeBase = Attribute.Base
module AttributeSpawn = Attribute.Spawn
module AttributeDamage = Attribute.Damage
module Attribute = Attribute.Api

open Core
(* open Quality *)

let spread_attribute total (features:Object.t Attribute.t array) =
  let ratio = Array.map (fun _ -> Random.int 10) features in
  let sum = Array.fold_left (fun acc c -> c + acc) 0 ratio in
  let ratio = Array.map (fun c -> (c * total) / sum) ratio in
  Array.map2 (fun amount wx -> Feature.mk_produce wx amount) ratio features

module BasicSystem = struct
  open AttributeBase
  let base_core = [|Attribute.Base Wu; Attribute.Base Ji; Attribute.Base Qi|]
  let make_features total = spread_attribute total base_core
end

module WuXingSystem = struct
  open Attribute
  open AttributeWuXing
  let wuxing_core = [|
        WuXing Jing;
        WuXing Mu;
        WuXing Shui;
        WuXing Huo;
        WuXing Tu;
    |]
  let make_features total = spread_attribute total wuxing_core
end
