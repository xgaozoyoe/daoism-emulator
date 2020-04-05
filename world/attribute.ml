type t = [`Jing | `Mu | `Shui| `Huo| `Tu ]

let to_string t = match t with
  | `Jing -> "Jing"
  | `Mu -> "Mu"
  | `Shui -> "Shui"
  | `Huo -> "Huo"
  | `Tu -> "Tu"
