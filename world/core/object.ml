open UID
type t = <
  get_name: string;
  get_env: (Feature.t * t)  Environ.t;
  get_command: Attribute.t option;
  set_command: Attribute.t option -> unit;
  take_features: Feature.t array -> unit;
  to_json: Yojson.Basic.t;
  step: t -> t Space.t -> (Feature.t * (t array) * t) list Lwt.t;
  handle_event: t -> t array -> Feature.t -> (Feature.t * (t array) * t) list Lwt.t
>

class virtual elt n = object

  val name = n
  val uid = UID.of_string n
  val mutable env: (Feature.t * t) Environ.t = Environ.empty []
  val mutable modifier: Modifier.t list = []
  val mutable command: Attribute.t option = None

  method get_name = name
  method get_env = env
  method get_command = command
  method set_command c = command <- c

  method take_features features =
    Array.iter (fun f ->
       Environ.proceed_feature f env
    ) features

  method virtual handle_event: t -> t array -> Feature.t -> (Feature.t * t array * t) list Lwt.t
  method virtual to_json: Yojson.Basic.t
  method virtual step: t -> t Space.t -> (Feature.t * t array * t) list Lwt.t
end

type 'a state_trans = 'a -> t -> t Space.t -> t -> 'a * Timer.slice

type obj_map = {
  get_obj: UID.t -> t
}

type pre_event = Feature.t * (t option)

let pre_event_to_json pe =
  let f, obj_opt = pe in
  let cs = ("feature", Feature.to_json f) in
  let cs = match obj_opt with
    | None -> [cs]
    | Some obj -> cs :: [("target", `String obj#get_name)]
  in
  `Assoc cs
