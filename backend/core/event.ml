type 'a t = {
  src: 'a;
  target: 'a;
  feature: 'a Feature.t;
}

let get_source t = t.src

let get_target t = t.target

let get_feature t = t.feature

let mk_event feature src target =
  {src = src; target=target; feature=feature}

let log_string to_str t =
  Printf.sprintf "<source: %s, target: %s, feature: %s>"
    (to_str (get_source t))
    (to_str (get_target t))
    (Feature.to_string (get_feature t))
