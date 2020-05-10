
type tile = {
  name:string;
  tid:string;
  hist:int;
} [@@bs.deriving abstract]

type tiles = {
   width: int;
   height: int;
   tiles: tile array;
}[@@bs.deriving abstract]

type tile_info = {
  tid:string;
  center: int * int;
}

module tile_info_map = Map.Make(String)

let map_info = {
  mutable tile_map = Map.

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

let build_npc x y key =
  Printf.sprintf "<text x='%d' y='%d'>%s</text>" x y key

let build_tiles left top (tiles_info:tiles) =
  Js.log tiles_info;
  let centers = build_centers left top (tiles_info |. widthGet) (tiles_info |. heightGet) in
  Js.log @@ Printf.sprintf "centers built";
  let svgs = Array.mapi (fun i c ->
     let info = (tiles_info |. tilesGet).(i) in
     let type_no = info |. tidGet in
     let txt = build_text c (info |. nameGet) in
     let svg = build_svg type_no (build_hexagon c) in
     txt ^ svg
  ) centers in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs

(*
let build_npcs npc_infos =
  let svgs = Array.mapi (fun i c ->
    let info = npc_infos.(i) in
    let tile = info |. tileGet in
    let tile_center = map_info.tiles.(tile) in
    let svg = build_npc_svg tile_center (info |. nameGet)
  ) npc_infos in
  Array.fold_left (fun acc c -> acc ^ c) "" svgs
*)
