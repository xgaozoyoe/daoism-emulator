module Attribute = Attribute.Api
type 'a t =
  | Consume of 'a Attribute.t * int
  | Produce of 'a Attribute.t * int
  | Hold of 'a Attribute.t * int

let mk_produce attribute amount =
  Produce (attribute, amount)

let mk_consume attribute amount =
  Consume (attribute, amount)

let mk_hold attribute amount =
  Hold (attribute, amount)

let to_string b = match b with
  | Consume (attr, n) -> Printf.sprintf "<%s: Consume %d>"
      (Attribute.to_string attr) n
  | Produce (attr, n) -> Printf.sprintf "<%s: Produce %d>"
      (Attribute.to_string attr) n
  | Hold (attr, n) -> Printf.sprintf "<%s: Hold %d>"
      (Attribute.to_string attr) n

let to_json b : Yojson.Basic.t =
  match b with
  | Consume (attr, n) -> `Assoc [("Consume", `String (Attribute.to_string attr)); ("Amount", `Int n)]
  | Produce (attr, n) -> `Assoc [("Produce", `String (Attribute.to_string attr)); ("Amount", `Int n)]
  | Hold (attr, n) -> `Assoc [("Hold", `String (Attribute.to_string attr)); ("Amount", `Int n)]
