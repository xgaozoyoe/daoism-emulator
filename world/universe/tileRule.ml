open Core
let tile_rule universe = function
  | Tiles.Default.Mountain -> [|Npc.Common.apprentice_rule (universe :> Object.t) |]
  | _ -> [||]
