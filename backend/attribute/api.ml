type notice =
  | Meet
  | Leave
  | Stay
  | Enter
  | Dead

let notice_to_string = function
  | Meet -> "Meet"
  | Leave -> "Leave"
  | Stay -> "Stay"
  | Enter -> "Enter"
  | Dead -> "Dead"

type 'a t =
  | Base of Base.t
  | WuXing of WuXing.t
  | Equipment of Equipment.t
  | Damage of Damage.t
  | Spawn of ('a Spawn.t)
  | Notice of notice

let to_string = function
  | Base att -> "Base." ^ Base.to_string att
  | Damage att -> "Damage." ^ Damage.to_string att
  | WuXing att -> "WuXing." ^ WuXing.to_string att
  | Spawn att -> "Spawn." ^ Spawn.to_string att
  | Notice att -> "Object." ^ notice_to_string att
  | Equipment att -> "Equipment." ^ Equipment.to_string att
