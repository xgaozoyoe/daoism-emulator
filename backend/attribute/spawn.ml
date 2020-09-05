type 'a t =
  | Apprentice of ('a -> 'a)
  | Creature of ('a -> 'a)
  | Mob of ('a -> 'a)
  | God of ('a -> 'a)
  | Building of ('a -> 'a)

let to_string p = match p with
    | Apprentice _ -> "Apprentice"
    | Creature _ -> "Creature"
    | Mob _ -> "Mob"
    | God _ -> "God"
    | Building _ -> "Building"
