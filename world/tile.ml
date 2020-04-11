module type Interface = sig

  type coordinate
  type tile
  type map

end


module Make (O:Object.Interface) = struct

  type coordinate = int * int

  type tile = O.t

  type map = tile array array

  let mkMap () = [||]

  let mk_tile name = O.mk_empty name name
(*
  let get_tile: coordinate -> tile
  get_distance: coordinate -> coordinate -> int
  fold_within: ('a -> tile -> 'a) -> coordinate -> distance -> 'a -> t -> 'a
  filt_within: (tile -> bool) -> coordinate -> distance -> t -> tile list
  fold: ('a tile -> 'a) -> 'a -> t -> 'a
  map: ('a tile -> 'a) -> 'a -> t -> 'a
*)

end

