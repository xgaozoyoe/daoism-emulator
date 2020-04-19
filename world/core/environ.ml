module type Interface = sig

  type t

  module ID: UID.Interface
  module Feature: Feature.Interface
  module Modifier: Modifier.Interface

  val empty: t
  val proceed_feature: Feature.t -> t -> t
  val fold: (ID.t -> (Attribute.t * int) -> 'a -> 'a)  -> 'a -> t -> 'a


end

module Make (ID:UID.Interface) (F:Feature.Interface) (M:Modifier.Interface) = struct

  module ID = ID
  module Feature = F
  module Modifier = M
  module FeatureMap = Map.Make(ID)

  type elt = Attribute.t * int

  type t = elt FeatureMap.t

  type rule = (t array * t)

  let update_entry key cb_exist cb_none env =
    let v = match FeatureMap.find_opt key env with
    | None -> cb_none
    | Some c -> cb_exist c
    in
    match v with
    | Some v -> FeatureMap.add key v env
    | None -> env

  let empty = FeatureMap.empty

  let fold f acc env = FeatureMap.fold f acc env

  let proceed_feature feature env =
    match feature with
    | F.Consume (attr, amount) ->
      update_entry (ID.of_string attr#name) (fun (attr,a) ->
        if a >= amount then Some (attr, a - amount) else Some (attr, 0)
      ) None env
    | F.Produce (attr, amount) ->
      update_entry (ID.of_string attr#name) (fun (attr,a) ->
        Some (attr, a + amount)
      ) (Some (attr, amount)) env
    | F.Hold (attr, amount) ->
      update_entry (ID.of_string attr#name) (fun _ ->
        Some (attr, amount)
      ) (Some (attr, amount)) env

  let apply pickers env = []
end

end
