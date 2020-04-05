module type Interface = sig
  type id
  type 'b t
end

module Make (ID:UID.Interface) = struct
  type id = ID.t
  type 'b t =
    | Consume of id * 'b * int
    | Produce of id * 'b * int
    | Hold of id * 'b * int
end


