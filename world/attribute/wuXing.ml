type t =
  | Jing (* 金 *)
  | Mu   (* 木 *)
  | Shui (* 水 *)
  | Huo  (* 火 *)
  | Tu   (* 土 *)

let to_string wx = match wx with
  | Jing -> "金"
  | Mu -> "木"
  | Shui -> "水"
  | Huo -> "火"
  | Tu -> "土"

let category _ = "WuXing"
