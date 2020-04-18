open Core
module Make (O:Object.Interface) = struct
  let make_state _ timeslice = fun _ ->
    ("Default", [], timeslice)

end
