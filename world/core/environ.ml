module ID = UID.Make(UID.Id)
module FeatureMap = Map.Make(ID)

exception NotEnough

type elt = Attribute.t * int

type 'a rule = elt array * 'a

type 'a t = {
  mutable features: elt FeatureMap.t;
  mutable rules: ('a rule) list;
}


let update_entry key cb_exist cb_none features =
  let v = match FeatureMap.find_opt key features with
  | None -> cb_none ()
  | Some c -> cb_exist c
  in
  match v with
  | Some v -> FeatureMap.add key v features
  | None -> features

let empty rules = { features = FeatureMap.empty; rules = rules }

let fold f acc env = FeatureMap.fold f env.features acc

let dump env = FeatureMap.fold (fun _ (attr,i) acc ->
    acc ^ " " ^ (Printf.sprintf "%s |-> %d" (attr#name) i)
  ) env.features ""

let proceed_feature feature env =
  match feature with
  | Feature.Consume (attr, amount) ->
    env.features <- update_entry attr#id (fun (attr,a) ->
      if a >= amount then Some (attr, a - amount) else Some (attr, 0)
    ) (fun _ -> None) env.features
  | Feature.Produce (attr, amount) ->
    env.features <- update_entry attr#id (fun (attr,a) ->
      Some (attr, a + amount)
    ) (fun _ -> Some (attr, amount)) env.features
  | Feature.Hold (attr, amount) ->
    env.features <- update_entry attr#id (fun _ ->
      Some (attr, amount)
    ) (fun _ -> Some (attr, amount)) env.features

let install_rule (rule:'a) env =
  env.rules <- rule :: env.rules

let apply_rules env =
  let features, fires = List.fold_left (fun (env, fs) (attrs_require, r) ->
    try
      let env = Array.fold_left (fun acc (attr,amount) ->
        update_entry attr#id
          (fun (attr, a) ->
            if a >= amount then Some (attr, a - amount) else raise NotEnough)
          (fun _ -> raise NotEnough)
          acc
      ) env attrs_require in
      env, r :: fs
    with NotEnough -> env, fs
  ) (env.features, []) env.rules in
  env.features <- features;
  fires
