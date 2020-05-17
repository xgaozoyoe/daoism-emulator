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
  type t = {
    name:string;
    tile:string;
  }[@@bs.deriving abstract]
end


module IdMap = Map.Make(String)

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

let build_svg r points =
  let (x0,y0), (x1,y1), (x2,y2), (x3,y3), (x4,y4), (x5, y5) = points in
  let points:string = Printf.sprintf "%d,%d %d,%d %d,%d %d,%d %d,%d %d,%d %d,%d"
    x0 y0 x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x0 y0
  in
  Printf.sprintf "<polygon class='hex_%s' points='%s'></polygon>" r points

let build_text center txt =
  Printf.sprintf "<text x='%d' y='%d'>%s</text>" (fst center) (snd center) txt

let build_feature center =
  Printf.sprintf "<text x='%d' y='%d'>%s</text>" (fst center) (snd center) "w"

let build_npc x y key =
  Printf.sprintf "<circle cx='%d' cy='%d' stroke='black' fill='white' r='5'></circle>" x y

let build_tiles left top tiles_info =
  let open Tile in
  Js.log tiles_info;
  let centers = build_centers left top (tiles_info |. widthGet) (tiles_info |. heightGet) in
  Js.log @@ Printf.sprintf "centers built";
  let svgs = Array.mapi (fun i c ->
     let info = (tiles_info |. tilesGet).(i) in
     let type_no = info |. ttypeGet |. baseGet in
     let name = info |. nameGet in
     map_info.tiles <- IdMap.add name (mk_tile type_no c) map_info.tiles;
     (* let txt = build_text c (info |. nameGet) in *)
     let svg = build_svg type_no (build_hexagon c) in
     let tile_feature = info |. ttypeGet |. featuresGet in
     Array.fold_left (fun svg f -> (build_feature c) ^ svg) svg tile_feature
  ) centers in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs

let build_npcs npc_infos =
  let open Npc in
  let svgs = Array.mapi (fun i c ->
    let info = npc_infos.(i) in
    let tile = info |. tileGet in
    let tile = IdMap.find tile map_info.tiles in
    let x, y = tile.center in
    let svg = build_npc x y (info |. nameGet) in
    svg
  ) npc_infos in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs
