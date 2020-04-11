module type Interface = sig

  type t
  type key
  type feature

  val to_string: t -> string
  val take_features: feature list -> t -> t
  val mk_empty: string -> string -> t

end


module Make (ID:UID.Interface) (Env: Environ.Interface) (M:Modifier.Interface)= struct

  type key = ID.t

  type feature = Env.feature

  type t = {
    name: string;
    uid: key;
    env: Env.t;
    modifier: M.t list;
  }

  let mk_empty name uid = {name = name; uid = ID.of_string uid; env = Env.empty; modifier = []}

  let to_string t = Printf.sprintf "<[%s]%s>" (ID.to_string t.uid) t.name

  let take_feature _ t = t

  let take_features features t = List.fold_left (fun acc f ->
    take_feature f acc
  ) t features

end


