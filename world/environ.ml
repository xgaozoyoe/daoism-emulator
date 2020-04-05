module type Interface = sig

  type elt
  type t

end

module Make (ID:UID.Interface) (F:Feature.Interface) (M:Modifier.Interface) = struct

  module FMap = Map.Make(ID)
  type elt = F.t
  type t = F.t FMap.t

end
