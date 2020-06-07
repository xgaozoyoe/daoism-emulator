type t =
  | Apprentice
  | Creature
  | Mob
  | God

let to_string p = match p with
    | Apprentice -> "Apprentice"
    | Creature -> "Creature"
    | Mob -> "Mob"
    | God -> "God"

let category _ = "Spawn"

