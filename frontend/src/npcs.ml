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

end

let build_npc info uinfo =
  let open Npc in
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = (uinfo |. widthGet)
    let height = (uinfo |. heightGet)
  end) in
  let state = info |. stateGet in
  let tile_cor = info |. locGet in
  let name = info |. nameGet in
  let desc = state |. descriptionGet in
  let cor = (tile_cor |. xGet), (tile_cor |. yGet) in
  let layout = HexCoordinate.layout cor in
  let svg_name = List.hd @@ String.split_on_char '.' name in
  let icon = SvgHelper.mk_use svg_name layout in
  let hint = SvgHelper.mk_text layout desc in
  SvgHelper.mk_group name (icon ^ hint)

let build_npcs npc_infos uinfo =
  let svgs = Array.mapi (fun i _ ->
    let info = npc_infos.(i) in
    let svg = build_npc info uinfo in
    svg
  ) npc_infos in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs


