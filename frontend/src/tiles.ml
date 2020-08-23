open SvgHelper
open Global

module Tile = struct

  type tile_type = {
    base:string;
    features: string array;
  } [@@bs.deriving abstract]

  type info = {
    name:string;
    ttype:tile_type;
    hist:int;
  } [@@bs.deriving abstract]

  let display_info info =
    [|
      "name", info |. nameGet;
      "type", info |. ttypeGet |. baseGet;
      "hist", Printf.sprintf "%d" (info |. histGet)
    |]

end

module WaterPath = struct

  exception UnexpectedPath

  let get_direction_point (cx, cy) =
    [|
        (cx, cy -. 26.); (cx -. 22.5, cy -. 13.)
      ; (cx -. 22.5, cy +. 13.); (cx, cy +. 26.)
      ; (cx +. 22.5, cy +. 13.); (cx +. 22.5, cy -. 13.)
    |]

  let make_path_svg (cx, cy) info = begin
    let direction_info = get_direction_point (cx, cy) in
    match info with
    | [p; q] -> begin
        let from_p = direction_info.(p) in
        let to_q = direction_info.(q) in
        mk_line from_p to_q (*cx, cy*)
      end
    | [p] -> begin
        let to_p = direction_info.(p) in
        mk_line (cx, cy) to_p
      end
    | _ -> raise UnexpectedPath
  end

end

let to_float (x,y) = (Js.Int.toFloat x, Js.Int.toFloat y)

let build_feature center f =
  let (x, y) = center in
  let feature_info = String.split_on_char '_' f in
  let feature_name = List.hd feature_info in
  let feature_info = List.map (fun c ->int_of_string c) (List.tl feature_info) in
  match feature_name with
  | "river" -> (WaterPath.make_path_svg (to_float center) feature_info)
  | _ -> Printf.sprintf "<g><use href='/dist/res/%s.svg#main' x='%d' y='%d' width='40' height='40'/></g>" feature_name x y

(* Make a single hexagon tile *)
let build_tile name typ_no pos =
  let style = Printf.sprintf "hex_%s" typ_no in
  SvgHelper.mk_hexagon name style pos

let handle_click info () =
  let menu = Document.get_by_id Document.document "menu" in
  let innerHTML = Menu.build_menu (Tile.display_info info) in
  Document.setInnerHTML menu innerHTML

let build_tiles tiles uinfo container=
  let open Tile in
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = (uinfo |. widthGet)
    let height = (uinfo |. heightGet)
  end) in
  Js.log tiles;
  let svgs = Array.mapi (fun i info ->
    let type_no = info |. ttypeGet |. baseGet in
    let name = info |. nameGet in
    let cor = HexCoordinate.from_index i in
    let layout = HexCoordinate.layout cor in
    let svg = build_tile name type_no layout in
    let tile_feature = info |. ttypeGet |. featuresGet in
    Array.fold_left (fun svg f -> svg ^ (build_feature layout f)) svg tile_feature
  ) tiles in
  let innerHTML = Array.fold_left (fun acc c -> acc ^ c) "" svgs in
  Document.setInnerHTML container innerHTML;
  (* Register event handler for tiles *)
  Array.map (fun info ->
    let name = info |. nameGet in
    let item = Document.get_by_id Document.document name in
    Document.add_event_listener item "click" (handle_click info)
  ) tiles
