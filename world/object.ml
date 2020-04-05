module type Interface = sig

  type t
  type key

  val to_string: t -> string

end


module Make (ID:UID.Interface) (Env: Environ.Interface) (M:Modifier.Interface)= struct

  type key = ID.t

  type t = {
    uid: key;
    env: Env.t;
    modifier: M.t list;
  }

  let to_string t = ID.to_string t.uid

end


