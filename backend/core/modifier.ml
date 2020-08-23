type 'a t = {
  filt: 'a Feature.t -> bool;
  batch: 'a Feature.t list -> int -> int
}

let mk_modifier filt batch = { filt = filt; batch = batch }
