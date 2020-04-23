type t = {
  src: Object.t ref;
  target: Object.t ref;
  feature: Feature.t;
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
    (Feature.to_string (get_feature t))
