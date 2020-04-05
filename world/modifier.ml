module type Interface = sig
  type 'a t =  < fit: 'a -> bool; batch_effect: 'a list -> int -> int >
  val mk_modifier: ('a -> bool) -> ('a list -> int -> int) -> 'a t
end

module Make (ID:UID.Interface) (F:Feature.Interface) = struct

  type t = {
    fit: F.t -> bool;
    batch: F.t list -> int -> int
  }

end
