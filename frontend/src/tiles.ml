open SvgHelper
open Global

module Tile = struct
  open Common

  type tile_type = {
    base:string;
    features: string array;
  } [@@bs.deriving abstract]


  type tile_info = {
    name:string;
    state:state;
    loc:location;
    ttype:tile_type;
    env:environ;
  } [@@bs.deriving abstract]

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
  | _ -> Printf.sprintf "<use href='/dist/res/%s.svg#main' x='%d' y='%d' width='40' height='40'/>"
    feature_name (x-20) (y-20)

(* Make a single hexagon tile *)
let build_tile i pos feature_svg =
  let open Tile in
  let name = i |. nameGet in
  let typ_no = i |. ttypeGet |. baseGet in
  let style = Printf.sprintf "hex_%s" typ_no in
  let svg = SvgHelper.mk_hexagon style pos in
  let svg = svg ^ feature_svg in
  mk_group name svg

let display_info info =
  let open Tile in
  (* let avatar = build_tile info in *)
  let avatar = fun _ -> "" in
  let i = Js.Dict.fromList [
    "name", info |. nameGet;
    "type", info |. ttypeGet |. baseGet;
  ] in
  let deliver = info |. stateGet |. Common.deliverGet in
  let rules = info |. envGet |. Common.rulesGet in
  Js.log (rules);
  Js.log (Array.length rules);
  let attrs = Js.Dict.fromList [] in
  let methods = Js.Dict.fromList [] in
  Menu.mk_menu (avatar,10) i attrs methods rules deliver


let handle_click info () =
  (*let menu = Document.get_by_id Document.document "menu" in *)
  let open Tile in
  let cor = (info |. locGet |. Common.xGet), (info |. locGet |. Common.yGet) in
  let reset = if (Action.state_wait_for_coordinate ()) then begin
      if (Action.feed_state (info |. nameGet) cor ) then begin
        Menu.reset_assist_menu ();
        false
      end else true
    end else true
  in
  if reset then begin
    Action.reset_state ();
    Menu.build_menu (info |. nameGet) (display_info info) (fun _ -> Action.({
    ids=[]; svg=""}))
  end


let get_tile_element idx =
  let id = "tile_" ^ (string_of_int idx) in
  Document.get_by_id Document.document id

let build_tiles tiles uinfo container=
  let open Tile in
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = (uinfo |. widthGet)
    let height = (uinfo |. heightGet)
  end) in
  Js.log tiles;
  let svgs = Array.mapi (fun i info ->
    let cor = HexCoordinate.from_index i in
    let layout = HexCoordinate.layout cor in
    let tile_feature = info |. ttypeGet |. featuresGet in
    let feature_svg = Array.fold_left (fun svg f -> svg ^ (build_feature layout f)) "" tile_feature in
    build_tile info layout feature_svg
  ) tiles in
  let innerHTML = Array.fold_left (fun acc c -> acc ^ c) "" svgs in
  Document.setInnerHTML container innerHTML;
  (* Register event handler for tiles *)
  Array.map (fun info ->
    let name = info |. nameGet in
    let item = Document.get_by_id Document.document name in
    Document.add_event_listener item "click" (handle_click info)
  ) tiles
