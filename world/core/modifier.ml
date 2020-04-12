module type Interface = sig
  type f
  type t
  val mk_modifier: (f -> bool) -> (f list -> int -> int) -> t
end

module Make (ID:UID.Interface) (F:Feature.Interface) = struct

  type f = F.t

  type t = {
    filt: F.t -> bool;
    batch: F.t list -> int -> int
  }

  let mk_modifier filt batch = { filt = filt; batch = batch }

end
