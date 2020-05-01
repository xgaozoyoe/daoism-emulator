module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
module AttributeBase = Core.Attribute.From(Attribute.Base)
module AttributeSpawn = Core.Attribute.From(Attribute.Spawn)

open Core

class tile_attr (obj:Object.t) = object
  inherit Attribute.attr
  method name = obj#get_name
  method category = "Move"
end

let mk_tile_attr t: Attribute.t = (new tile_attr t :> Attribute.t)

let apprentice_rule oref : (Feature.t * Object.t) Environ.rule =
    let apprentice = new AttributeSpawn.ext_attr Apprentice in
    [| new AttributeWuXing.ext_attr Jing, 10 |]
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

let practise_state quality state =
   let open Api in
   { state with
     description = "修炼";
     features = Array.map (fun x-> x, None) (make_basic_features (quality_to_total quality));
     last = Timer.of_int 10;
   }

let explore_state state universe =
   let open Api in
   { state with
     description = "探索";
     features = [|Feature.mk_hold (mk_tile_attr state.tile) 1, Some universe|];
     last = Timer.of_int 5;
   }

let move_state state tile =
   let open Api in
   {
     description = "移动";
     features = [|Feature.mk_hold (mk_tile_attr tile) 1, Some tile|];
     last = Timer.of_int 5;
     tile = state.tile
   }

let make_state quality = fun (state,s) ->
  let pick = Random.int 2 in
  match pick with
    | 1 -> practise_state quality state
    | 0 -> explore_state state s
    | _ -> assert false

let mk_apprentice tile =
    let name = Core.Name.gen_name "Apprentice" in
    new Api.elt name (make_state Quality.Normal) tile
