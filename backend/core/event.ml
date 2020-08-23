type t = {
  src: Object.t array;
  target: Object.t;
  feature: Object.t Feature.t;
}

let get_source t = t.src

let get_target t = t.target

let get_feature t = t.feature

let mk_event feature src target =
  {src = src; target=target; feature=feature}

let to_string t =
  Printf.sprintf "<source: %s, target: %s, feature: %s>"
    (Array.fold_left (fun acc c -> acc ^ " " ^
        c#get_name) "" (get_source t))
    ((get_target t)#get_name)
    (Feature.to_string (get_feature t))
