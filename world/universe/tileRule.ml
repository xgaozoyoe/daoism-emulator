open Core
open Tiles.Default
let tile_rule universe ttype = match ttype.base with
  | Tiles.Default.Mountain -> [|Npc.Apprentice.apprentice_rule (universe :> Object.t) |]
  | _ -> [||]