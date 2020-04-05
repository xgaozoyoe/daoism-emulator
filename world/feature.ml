module type Interface = sig
  type id
  type t
  val to_string: t -> string
end

module Make (ID:UID.Interface) = struct
  type id = ID.t
  type t =
    | Consume of id * Attribute.t * int
    | Produce of id * Attribute.t * int
    | Hold of id * Attribute.t * int

  let to_string b = match b with
    | Consume (id, attr, n) -> Printf.sprintf "[%s] <%s: Consume %d>"
        (ID.to_string id) (Attribute.to_string attr) n
    | Produce (id, attr, n) -> Printf.sprintf "[%s] <%s: Produce %d>"
        (ID.to_string id) (Attribute.to_string attr) n
    | Hold (id, attr, n) -> Printf.sprintf "[%s] <%s: Hold %d>"
        (ID.to_string id) (Attribute.to_string attr) n
end


