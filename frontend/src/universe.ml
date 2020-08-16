open Tiles
open Npcs

type position = (int * int)

let get_the = function
  | Some a -> a
  | None -> raise Not_found

let initialize_coordinate _ _ _ = ()

module IdMap = Map.Make(String)
module SVGMap = Map.Make(String)

type t = {
  mutable tiles: Tile.info IdMap.t;
  mutable npcs: Npc.t IdMap.t;
}

let map_info:t = {
  tiles = IdMap.empty;
  npcs = IdMap.empty;
}

let build_menu _ =
  Printf.sprintf "<use href='/dist/res/menu.svg#main' x='%d' y='%d'/>" 10 10
