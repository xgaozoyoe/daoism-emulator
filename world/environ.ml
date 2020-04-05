module type Interface = sig

  type t

end

module Make (ID:UID.Interface) (F:Feature.Interface) (M:Modifier.Interface) = struct

  module FMap = Map.Make(ID)
  type t = F.t FMap.t

end
