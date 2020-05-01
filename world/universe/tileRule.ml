open Core
let tile_rule universe = function
  | Tiles.Default.Mountain -> [|Npc.Apprentice.apprentice_rule (universe :> Object.t) |]
  | _ -> [||]
