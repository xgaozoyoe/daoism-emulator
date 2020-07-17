type t = {
  filt: Feature.t -> bool;
  batch: Feature.t list -> int -> int
}

let mk_modifier filt batch = { filt = filt; batch = batch }
