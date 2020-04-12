module type Event = sig

  module Object:Object.Interface

  type t

  val mk_event: Object.t ref -> Object.Env.Feature.t -> t
  val get_source: t -> Object.t
  val get_feature: t -> Object.Env.Feature.t
  val to_string: t -> string

end

module Make (O:Object.Interface) = struct

  module Object = O

  type t = (O.t ref) * (O.Env.Feature.t)

  let get_source t = !(fst t)

  let get_feature t = snd t

  let mk_event src feature : t = (src, feature)

  let to_string t =
    Printf.sprintf "<source: %s, feature: %s>"
      (O.to_string (get_source t)) (O.Env.Feature.to_string (get_feature t))

end
