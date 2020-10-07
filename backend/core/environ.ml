module ID = UID.UID
module FeatureMap = Map.Make(ID)
module Attribute = Attribute.Api

exception NotEnough

type 'a elt = 'a Attribute.t * int

(* A rule sends 'a elt to 'a *)
type 'a rule = ('a elt) array * ('a Feature.t * 'a)

type 'a t = {
  mutable features: ('a elt) FeatureMap.t;
  mutable bounds: ('a elt) FeatureMap.t;
  mutable rules: ('a rule) list;
}

let update_entry attr cb_exist cb_none features =
  let key = ID.of_string @@ Attribute.to_string attr in
  let v = match FeatureMap.find_opt key features with
  | None -> cb_none ()
  | Some c -> cb_exist c
  in
  match v with
  | Some v -> FeatureMap.add key v features
  | None -> features

let empty rules = { features = FeatureMap.empty; bounds = FeatureMap.empty; rules = rules }

let fold f acc env = FeatureMap.fold f env.features acc

let dump env =
  let c = FeatureMap.fold (fun _ (attr,i) acc ->
    acc ^ " " ^ (Printf.sprintf "%s |-> %d" (Attribute.to_string attr) i)
  ) env.features "" in
  List.fold_left (fun acc (attrs, _) ->
    let require = Array.fold_left (fun acc (attr,i) ->
      Printf.sprintf "%s , %s : %d" acc (Attribute.to_string attr) i
    ) "" attrs in
    acc ^ ", <" ^ require ^ ">"
  ) c env.rules

let elt_to_json (attr, n) =
  `Assoc [(Attribute.to_string attr, `Int n)]

let to_json (env: 'a t) : Yojson.Basic.t =
  let fs = FeatureMap.fold (fun _ (attr,i) acc ->
    acc @ [(Attribute.to_string attr, `Int i)]
  ) env.features [] in
  let rules = List.fold_left (fun acc (attrs, (f, _)) ->
    let require = Array.fold_left (fun acc (attr,i) ->
      acc @ [(Attribute.to_string attr, `Int i)]
    ) [] attrs in
    let result = Feature.to_string f in
    acc @ [`Assoc [("require", `Assoc require); ("result", `String result)]]
  ) [] env.rules in
  `Assoc [("features", `Assoc fs); ("rules", `List rules)]

let set_bound (attr, amount) env =
  env.bounds <- update_entry attr (fun _ -> Some (attr, amount))
    (fun _ -> Some (attr, amount)) env.bounds

let proceed_feature feature env =
  match feature with
  | Feature.Consume (attr, amount) ->
    env.features <- update_entry attr (fun (attr,a) ->
      if a >= amount then Some (attr, a - amount) else Some (attr, 0)
    ) (fun _ -> None) env.features
  | Feature.Produce (attr, amount) ->
    let bound = FeatureMap.find_opt (ID.of_string (Attribute.to_string attr)) env.bounds in
    let set v = match bound with
    | None -> v
    | Some (_, b) -> if v < b then v else b
    in
    env.features <- update_entry attr (fun (attr,a) ->
      Some (attr, set (a + amount))
    ) (fun _ -> Some (attr, set (amount))) env.features
  | Feature.Hold (attr, amount) ->
    env.features <- update_entry attr (fun _ ->
      Some (attr, amount)
    ) (fun _ -> Some (attr, amount)) env.features

let replace_feature (attr, amount) env =
  let rep = ref None in
  env.features <- update_entry attr (fun (attr', amount') -> begin
      rep := Some (attr', amount') ;
      Some (attr, amount)
    end
  ) (fun _ -> Some (attr, amount)) env.features;
  !rep

let install_rule (rule:'a) env =
  env.rules <- rule :: env.rules

let apply_rules env =
  let features, fires = List.fold_left (fun (env, fs) (attrs_require, r) ->
    try
      let env = Array.fold_left (fun acc (attr,amount) ->
        update_entry attr
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
