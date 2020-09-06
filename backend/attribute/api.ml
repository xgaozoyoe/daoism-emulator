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
  | Damage of Damage.t
  | Spawn of ('a Spawn.t)
  | Object of notice

let to_string = function
  | Base att -> "Base." ^ Base.to_string att
  | Damage att -> "Damage." ^ Damage.to_string att
  | WuXing att -> "WuXing." ^ WuXing.to_string att
  | Spawn att -> "Spawn." ^ Spawn.to_string att
  | Object att -> "Object." ^ notice_to_string att

let same_category a b = match (a, b) with
  | Base _, Base _ -> true
  | Damage _, Damage _ -> true
  | WuXing _, WuXing _ -> true
  | Spawn _, Spawn _ -> true
  | _ -> false
