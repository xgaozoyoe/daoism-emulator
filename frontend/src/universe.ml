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

let fetch_data () = Action.send_fetch_data ()

type obj_info = {
  name: string;
}[@@bs.deriving abstract]

let update_obj obj_info uinfo =
  let npcs_outter = Document.get_by_id Document.document "npcs" in
  let name = obj_info |. nameGet in
  let id = UID.UID.of_string name in
  let module_name = UID.UID.get_module_name id in
  match module_name with
  | "Tile" -> Tiles.update_tile (Document.cast Document.daoism obj_info)
  | _ -> Npcs.update_npc (Document.cast Document.daoism obj_info) uinfo npcs_outter
