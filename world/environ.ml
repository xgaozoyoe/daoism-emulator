module type Interface = sig

  type feature
  type t

  val empty: t

end

module Make (ID:UID.Interface) (F:Feature.Interface) (M:Modifier.Interface) = struct

  module FeatureMap = Map.Make(ID)

  type feature = F.t
  type t = F.t FeatureMap.t

  let empty = FeatureMap.empty

end
