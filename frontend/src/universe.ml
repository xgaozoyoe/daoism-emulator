type position = (int * int)

let get_the = function
  | Some a -> a
  | None -> raise Not_found

let initialize_coordinate _ _ _ = ()

module IdMap = Map.Make(String)
module SVGMap = Map.Make(String)

type t = {
  mutable tiles: Tiles.Tile.tile_info IdMap.t;
  mutable npcs: Npcs.Npc.t IdMap.t;
}

let map_info:t = {
  tiles = IdMap.empty;
  npcs = IdMap.empty;
}

let build_tiles = Tiles.build_tiles
let build_npcs = Npcs.build_npcs
let add_npc = Npcs.add_npc
let update_npc = Npcs.update_npc

let fetch_data () = Action.send_fetch_data ()
