module type Event = sig

  module Object:Object.Interface

  type t

  val mk_event: Object.t ref -> Object.Env.Feature.t -> Object.t ref -> t
  val get_source: t -> Object.t
  val get_feature: t -> Object.Env.Feature.t
  val get_target: t -> Object.t
  val to_string: t -> string

end

module Make (O:Object.Interface) = struct

  module Object = O

  type t = {
    src: O.t ref;
    target: O.t ref;
    feature: O.Env.Feature.t;
  }

  let get_source t = !(t.src)

  let get_target t = !(t.target)

  let get_feature t = t.feature

  let mk_event src target feature =
    {src = src; target=target; feature=feature}

  let to_string t =
    Printf.sprintf "<source: %s, target: %s, feature: %s>"
      ((get_source t)#get_name)
      ((get_target t)#get_name)
      (O.Env.Feature.to_string (get_feature t))

end
