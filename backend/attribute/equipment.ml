type equip_id = int

type t =
  | Armor of equip_id
  | Weapon of equip_id

let to_string p = match p with
  | Armor _ -> "Armor"
  | Weapon _ -> "Weapon"
