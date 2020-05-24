module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
open Common

type tile_feature =
  | River of int list

let feature_id = function
  | River ls -> List.fold_left (fun acc c -> Printf.sprintf "%s_%d" acc c) "river" ls

type base_type =
  | Plain
  | Water
  | Grassland
  | Forest
  | Mountain

let base_id = function
  | Plain-> "plain"
  | Water-> "water"
  | Grassland -> "grassland"
  | Mountain -> "mountain"
  | Forest -> "forest"

type tile_type = {
  base: base_type;
  features: tile_feature list;
}

let init_tile base = {base=base; features=[]}

let add_feature f tile = {tile with features = f :: tile.features}

let to_json ttyp =
  `Assoc [
  ("base", `String (base_id ttyp.base))
  ; ("features", `List (List.map (fun f->
      `String (feature_id f)) ttyp.features))
  ]

let to_string = function
  | Water-> "水"
  | Grassland -> "草"
  | Mountain -> "山"
  | Forest -> "林"
  | Plain-> "无"

let tile_type_array = [|Mountain; Water; Grassland; Forest |]

let make_default_state _ ttyp timeslice = fun state _ _ ->
  let open Core in
  let open AttributeWuXing in
  let es = match ttyp.base with
  | Mountain -> [|Feature.mk_produce (new ext_attr Jing) 1|]
  | Water -> [|Feature.mk_produce (new ext_attr Shui) 1|]
  | Grassland -> [|Feature.mk_produce (new ext_attr Tu) 1|]
  | Forest -> [|Feature.mk_produce (new ext_attr Mu) 1|]
  | _ -> [||]
  in
  { state with deliver = Array.map (fun es -> (es, None)) es }, timeslice

let set_default_bound _ _ bound tile =
  let open AttributeWuXing in
  let open Core in
  let bounds = [| (new ext_attr Jing), bound
    ; (new ext_attr Shui), bound
    ; (new ext_attr Tu), bound
    ; (new ext_attr Huo), bound
    ; (new ext_attr Mu), bound
    |] in
  Array.iter (fun b -> Environ.set_bound b tile#get_env) bounds
