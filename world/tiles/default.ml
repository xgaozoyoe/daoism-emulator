module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)

type tile_type =
  | Mountain
  | River
  | Grassland
  | Forest
  | Cave

let to_string = function
  | Mountain -> "山"
  | River -> "河"
  | Grassland -> "草"
  | Forest -> "林"
  | Cave -> "洞"

let tile_type_array = [|Mountain; River; Grassland; Cave|]

let make_state _ ttyp timeslice = fun _ ->
   let open Core in
   let open AttributeWuXing in
   let name = to_string ttyp in
   match ttyp with
   | Mountain ->
     let f = Feature.mk_produce (new ext_attr Jing) 1 in
     (name, [| (f, None) |], timeslice)
   | River ->
     let f = Feature.mk_produce (new ext_attr Shui) 1 in
     (name, [| (f, None) |], timeslice)
   | Grassland ->
     let f = Feature.mk_produce (new ext_attr Tu) 1 in
     (name, [| (f, None) |], timeslice)
   | Cave ->
     let f = Feature.mk_produce (new ext_attr Huo) 1 in
     (name, [| (f, None) |], timeslice)
   | Forest ->
     let f = Feature.mk_produce (new ext_attr Mu) 1 in
     (name, [| (f, None) |], timeslice)
