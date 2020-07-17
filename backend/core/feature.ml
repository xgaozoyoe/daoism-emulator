type t =
  | Consume of Attribute.t * int
  | Produce of Attribute.t * int
  | Hold of Attribute.t * int

let mk_produce attribute amount =
  Produce (attribute, amount)

let mk_consume attribute amount =
  Consume (attribute, amount)

let mk_hold attribute amount =
  Hold (attribute, amount)

let to_string b = match b with
  | Consume (attr, n) -> Printf.sprintf "<%s: Consume %d>"
      (attr#name) n
  | Produce (attr, n) -> Printf.sprintf "<%s: Produce %d>"
      (attr#name) n
  | Hold (attr, n) -> Printf.sprintf "<%s: Hold %d>"
      (attr#name) n

let to_json b : Yojson.Basic.t =
  match b with
  | Consume (attr, n) -> `Assoc [("Consume", `String attr#name); ("Amount", `Int n)]
  | Produce (attr, n) -> `Assoc [("Produce", `String attr#name); ("Amount", `Int n)]
  | Hold (attr, n) -> `Assoc [("Hold", `String attr#name); ("Amount", `Int n)]
