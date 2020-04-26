type t =
  | Apprentice
  | Mob
  | God

let to_string p = match p with
    | Apprentice -> "学徒"
    | Mob -> "妖怪"
    | God -> "神仙"

let category _ = "Npc"

