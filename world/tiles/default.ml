module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)

type tile_type =
  | Water
  | Grassland
  | Forest
  | Mountain

let to_string = function
  | Water-> "水"
  | Grassland -> "草"
  | Mountain -> "山"
  | Forest -> "林"

let type_id = function
  | Water-> "water"
  | Grassland -> "grassland"
  | Mountain -> "mountain"
  | Forest -> "forest"

let tile_type_array = [|Mountain; Water; Grassland; Forest |]

let make_default_state _ ttyp timeslice = fun (_, _, _) ->
  let open Core in
  let open AttributeWuXing in
  let name = to_string ttyp in
  match ttyp with
  | Mountain ->
    let f = Feature.mk_produce (new ext_attr Jing) 1 in
    (name, [| (f, None) |], timeslice)
  | Water ->
    let f = Feature.mk_produce (new ext_attr Shui) 1 in
    (name, [| (f, None) |], timeslice)
  | Grassland ->
    let f = Feature.mk_produce (new ext_attr Tu) 1 in
    (name, [| (f, None) |], timeslice)
  | Forest ->
    let f = Feature.mk_produce (new ext_attr Mu) 1 in
    (name, [| (f, None) |], timeslice)

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
