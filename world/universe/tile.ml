open Core

module type Interface = sig

  type coordinate
  type tile
  type map

  val mk_map: int -> int -> map
  val mk_tile: string -> tile

end


module Make (O:Object.Interface) : Interface = struct

  module Object = O

  type coordinate = int * int

  type tile = Object.t option

  type map = tile array

  let mk_tile name = Some (O.mk_empty name name)

  let mk_map width height : map =
    let map = Array.make (width * height) None in
    for i = 0 to width * height do
      map.(i) <- mk_tile (Printf.sprintf "t%d" i)
    done;
    map

end

