open Core
open Tiles.Default
let rule_of_feature universe f = match f with
  | Peak _ -> [ Npc.Apprentice.apprentice_rule (universe :> Object.t) ]
  | Cave _ -> [ Npc.Creature.creature_rule (universe :> Object.t) ]
  | _ -> []

let tile_rule universe ttype =
  Array.of_list @@ List.fold_left (fun acc f ->
    acc @ (rule_of_feature universe f)
  ) [] ttype.features
