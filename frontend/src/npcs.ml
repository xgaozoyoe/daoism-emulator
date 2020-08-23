open Global
module Npc = struct

  type location = {
    x:int;
    y:int;
  }[@@bs.deriving abstract]

  type state = {
    description:string;
  }[@@bs.deriving abstract]

  type t = {
    name:string;
    state:state;
    loc:location;
  }[@@bs.deriving abstract]

  let display_info info =
    [|
      "name", info |. nameGet;
      "state", info |. stateGet |. descriptionGet;
    |]


end

let handle_click info () =
  let menu = Document.get_by_id Document.document "menu" in
  let innerHTML = Menu.build_menu (Npc.display_info info) in
  Document.setInnerHTML menu innerHTML

let build_npc_content info uinfo =
  let open Npc in
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = (uinfo |. widthGet)
    let height = (uinfo |. heightGet)
  end) in
  let tile_cor = info |. locGet in
  let name = info |. nameGet in
  let cor = (tile_cor |. xGet), (tile_cor |. yGet) in
  let layout = HexCoordinate.layout cor in
  let svg_name = List.hd @@ String.split_on_char '.' name in
  let icon = SvgHelper.mk_use svg_name layout in
  (*
  let state = info |. stateGet in
  let desc = state |. descriptionGet in
  let hint = SvgHelper.mk_text layout desc in
  *)
  icon

let build_npc info uinfo =
  let open Npc in
  let content = build_npc_content info uinfo in
  SvgHelper.mk_group (info |. nameGet) content

let update_npc info uinfo =
  let open Npc in
  let name = info |. nameGet in
  let item = Document.get_by_id Document.document name in
  Document.setInnerHTML item (build_npc_content info uinfo);
  Document.add_event_listener item "click" (handle_click info)

let add_npc info uinfo container =
  let npc_svg = build_npc info uinfo in
  let item = Document.createElement Document.document npc_svg in
  Document.add_event_listener item "click" (handle_click info);
  Document.appendChild container item

let build_npcs npcs uinfo container =
  let open Npc in
  let svgs = Array.mapi (fun i _ ->
    let info = npcs.(i) in
    let svg = build_npc info uinfo in
    svg) npcs in
  let innerHTML = Array.fold_left (fun acc c -> acc ^ c) "" svgs in
  Document.setInnerHTML container innerHTML;
  (* Register event handler for tiles *)
  Array.map (fun info ->
    let name = info |. nameGet in
    let item = Document.get_by_id Document.document name in
    Document.add_event_listener item "click" (handle_click info)
  ) npcs


