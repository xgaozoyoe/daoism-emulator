module type Interface = sig

  type t =
    | Consume of Attribute.t * int
    | Produce of Attribute.t * int
    | Hold of Attribute.t * int

  val mk_produce: Attribute.t -> int -> t
  val mk_consume: Attribute.t -> int -> t
  val mk_hold: Attribute.t -> int -> t

  val to_string: t -> string
end

module Make (ID:UID.Interface) : Interface = struct

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
end


