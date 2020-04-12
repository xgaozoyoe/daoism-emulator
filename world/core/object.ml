module type Interface = sig

  module Env: Environ.Interface

  type t

  val to_string: t -> string
  val mk_empty: string -> string -> t
  val take_features: Env.Feature.t list -> t -> t
  val step: unit -> Env.Feature.t list

end

module Make (ID:UID.Interface) (Env: Environ.Interface) (M:Modifier.Interface)= struct

  module Env = Env

  type t = {
    name: string;
    uid: ID.t;
    env: Env.t;
    modifier: M.t list;
  }

  let mk_empty name uid = {name = name; uid = ID.of_string uid; env = Env.empty; modifier = []}

  let to_string t = Printf.sprintf "<[%s]%s>" (ID.to_string t.uid) t.name

  let take_feature _ t = t

  let take_features features t = List.fold_left (fun acc f ->
    take_feature f acc
  ) t features

  let step _ = []

end


