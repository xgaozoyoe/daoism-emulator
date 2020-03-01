module Coordinate : sig
    type t;
end

type 'a 'b t; (* 'a resource, 'b mob *)
val add_resource: 'a -> Coordinate.t -> unit
val remove_resource: 'a -> unit
val add_mob: 'b -> Coordinate.t -> unit
val remove_mob: 'b -> unit
val path: Coordinate.t -> Coordinate.t -> Coordinate.t array


