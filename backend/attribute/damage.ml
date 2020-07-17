type t =
  | Straight
  | Debuf

let to_string p = match p with
  | Straight -> "直接伤害"
  | Debuf -> "持续伤害"

let category _ = "Damage"

