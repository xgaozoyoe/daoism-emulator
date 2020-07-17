open HexCoordinate
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

  type t = {
    tid:string;
    center: int * int;
  }

  type tiles = {
     width: int;
     height: int;
     tiles: info array;
  }[@@bs.deriving abstract]

  let mk_tile tid center = {tid=tid; center= center}
end

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

type coordinate_info = {
  cor_to_index: (int * int) -> int;
  cor_to_pos: (int * int) -> (int * int);
}

let coordinate_system = ref None

let get_the = function
  | Some a -> a
  | None -> raise Not_found


let initialize_coordinate left top tiles_info =
  let open Tile in
  let width = (tiles_info |. widthGet) in
  let coordinate_to_pos (x, y) =
    let cx = x*45 in
    let cy = y*52 in
    let cy = if x mod 2 = 0 then cy else cy + 26 in
    cx+left, cy+top
  in
  let coordinate_to_index (x, y) = y*width + x
  in
  coordinate_system := Some {
    cor_to_pos = coordinate_to_pos;
    cor_to_index = coordinate_to_index;
  }

module IdMap = Map.Make(String)
module SVGMap = Map.Make(String)

type t = {
  mutable tiles: Tile.t IdMap.t;
  mutable npcs: Npc.t IdMap.t;
}

let map_info:t = {
  tiles = IdMap.empty;
  npcs = IdMap.empty;
}

let build_hexagon (cx, cy) =
  ((cx-30, cy), (cx-15, cy-26), (cx+15, cy-26),
    (cx+30, cy), (cx+15, cy+26), (cx-15, cy+26))

let get_direction_point (cx, cy) =
  [|
      (cx, cy -. 26.); (cx -. 22.5, cy -. 13.)
    ; (cx -. 22.5, cy +. 13.); (cx, cy +. 26.)
    ; (cx +. 22.5, cy +. 13.); (cx +. 22.5, cy -. 13.)
  |]

let build_hexagon_map ls_centers =
  Array.map (fun x -> build_hexagon x) ls_centers


let build_centers left top width height =
  let ls_centers = Array.init (width * height) (fun _ -> (0,0)) in
  for y=0 to height - 1 do
    for x=0 to width - 1 do
      let cx = x*45 in
      let cy = y*52 in
      let cy = if x mod 2 = 0 then cy else cy + 26 in
      ls_centers.(y *width + x) <- (cx+left, cy+top)
    done
  done;
  ls_centers

let build_tile name r points =
  let (x0,y0), (x1,y1), (x2,y2), (x3,y3), (x4,y4), (x5, y5) = points in
  let points:string = Printf.sprintf "%d,%d %d,%d %d,%d %d,%d %d,%d %d,%d %d,%d"
    x0 y0 x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x0 y0
  in
  Printf.sprintf "<polygon id='%s' class='hex_%s' points='%s'></polygon>" name r points

let build_text center txt =
  Printf.sprintf "<text x='%d' y='%d'>%s</text>" (fst center) (snd center) txt

let build_line (x1,y1) (x2,y2) =
  Printf.sprintf "<line x1='%f' y1='%f' x2='%f' y2='%f' class='river' />"
    x1 y1 x2 y2

let build_arc_line (x1,y1) (x2,y2) (cx,cy) =
  Printf.sprintf "<path d='M %f %f Q %f %f, %f %f' style='stroke:blue; stroke-width:2'/>"
    x1 y1 cx cy x2 y2

module WaterPath = struct

  exception UnexpectedPath

  let make_path_svg (cx, cy) info = begin
    let direction_info = get_direction_point (cx, cy) in
    match info with
    | [p; q] -> begin
        let from_p = direction_info.(p) in
        let to_q = direction_info.(q) in
        build_line from_p to_q (*cx, cy*)
      end
    | [p] -> begin
        let to_p = direction_info.(p) in
        build_line (cx, cy) to_p
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

let build_npc info =
  let open Npc in
  let state = info |. stateGet in
  let tile_cor = info |. locGet in
  let desc = state |. descriptionGet in
  let (x,y) = (tile_cor |. xGet), (tile_cor |. yGet) in
  let name = info |. nameGet in
  let (cx,cy) = (get_the !coordinate_system).cor_to_pos (x, y) in
  let svg_name = List.hd @@ String.split_on_char '.' name in
  let icon = Printf.sprintf "<use href='/dist/res/%s.svg#main' x='%d' y='%d' width='30' height='30'/>"
    svg_name cx cy in
  let hint = build_text (cx, cy) desc in
  Printf.sprintf "<g id='%s'>%s</g>" name (icon^hint)

let update_npc info =
  let open Npc in
  let state = info |. stateGet in
  let desc = state |. descriptionGet in
  let name = info |. nameGet in
  let tile_cor = info |. locGet in
  let (x,y) = (tile_cor |. xGet), (tile_cor |. yGet) in
  let (cx,cy) = (get_the !coordinate_system).cor_to_pos (x, y) in
  let svg_name = List.hd @@ String.split_on_char '.' name in
  let icon = Printf.sprintf "<use href='/dist/res/%s.svg#main' x='%d' y='%d' width='30' height='30'/>"
    svg_name cx cy in
  let hint = build_text (cx, cy) desc in
  Printf.sprintf "<g id='%s'>%s</g>" name (icon^hint)



let build_tiles left top tiles_info =
  let open Tile in
  Js.log tiles_info;
  let centers = build_centers left top (tiles_info |. widthGet) (tiles_info |. heightGet) in
  let svgs = Array.mapi (fun i c ->
     let info = (tiles_info |. tilesGet).(i) in
     let type_no = info |. ttypeGet |. baseGet in
     let name = info |. nameGet in
     map_info.tiles <- IdMap.add name (mk_tile type_no c) map_info.tiles;
     (* let txt = build_text c (info |. nameGet) in *)
     let svg = build_tile name type_no (build_hexagon c) in
     let tile_feature = info |. ttypeGet |. featuresGet in
     Array.fold_left (fun svg f -> svg ^ (build_feature c f)) svg tile_feature
  ) centers in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs

let build_npcs npc_infos =
  let open Npc in
  let svgs = Array.mapi (fun i _ ->
    let info = npc_infos.(i) in
    let svg = build_npc info in
    svg
  ) npc_infos in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs

let build_menu _ =
  Printf.sprintf "<use href='/dist/res/menu.svg#main' x='%d' y='%d'/>" 10 10
