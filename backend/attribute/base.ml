type t =
  | Wu (* 悟性 *)
  | Ji (* 机缘 *)
  | Qi (* 潜质 *)

let to_string p = match p with
    | Wu -> "悟性"
    | Ji -> "根基"
    | Qi -> "机缘"

let category _ = "Base"

