open Core

module AttrApi = Attribute.Api

type potential =
  | Wu (* 悟性 *)
  | Ji (* 机缘 *)
  | Qi (* 潜质 *)

class growth p = object (self)
  method name = match p with
    | Wu -> "Wu"
    | Ji -> "Ji"
    | Qi -> "Qi"
  method category = "Growth"
  method test (f:AttrApi.t) = f#category = self#category
end

let quality_to_total = function
| Quality.Unique -> 200
| Quality.Rare -> 150
| Quality.Elite -> 100
| Quality.Normal -> 80
| Quality.Antique -> Random.int 200

let make_basic_features total =
  let open Attribute.WuXing in
  let ratio = [|Random.int 10; Random.int 10; Random.int 10; Random.int 10|] in
  let sum = Array.fold_left (fun acc c -> c + acc) 0 ratio in
  let ratio = Array.map (fun c -> (c * total) / sum) ratio in
  let wx = [|
      new wuXing Jing;
      new wuXing Mu;
      new wuXing Shui;
      new wuXing Huo;
      new wuXing Tu;
  |] in
  Array.map2 (fun amount wx -> Feature.mk_produce wx amount) ratio wx

let make_state quality timeslice = fun _ ->
  let spawn = "Spawn" in
  let features_array =
    Array.map (fun x-> x, None) (make_basic_features (quality_to_total quality)) in
  (spawn, features_array, timeslice)
