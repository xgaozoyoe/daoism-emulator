module ID = UID.Make(UID.Id)
module Attribute = Attribute.Api
module FeatureMap = Map.Make(ID)

type elt = Attribute.t * int

type rule = (elt array * elt)

type t = {
  mutable features: elt FeatureMap.t;
  rules: rule list;
}


let update_entry key cb_exist cb_none env =
  let v = match FeatureMap.find_opt key env.features with
  | None -> cb_none
  | Some c -> cb_exist c
  in
  match v with
  | Some v -> env.features <- FeatureMap.add key v env.features
  | None -> ()

let empty = { features = FeatureMap.empty; rules = [] }

let fold f acc env = FeatureMap.fold f env.features acc

let proceed_feature feature env =
  match feature with
  | Feature.Consume (attr, amount) ->
    update_entry (ID.of_string attr#name) (fun (attr,a) ->
      if a >= amount then Some (attr, a - amount) else Some (attr, 0)
    ) None env
  | Feature.Produce (attr, amount) ->
    update_entry (ID.of_string attr#name) (fun (attr,a) ->
      Some (attr, a + amount)
    ) (Some (attr, amount)) env
  | Feature.Hold (attr, amount) ->
    update_entry (ID.of_string attr#name) (fun _ ->
      Some (attr, amount)
    ) (Some (attr, amount)) env

let apply_rules _ = []
