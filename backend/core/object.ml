open UID
open Utils
module Attribute = Attribute.Api
type t = <
  get_name: string;
  get_uid: UID.t;
  get_loc: HexCoordinate.t;
  set_loc: HexCoordinate.t -> unit;
  get_env: t Environ.t;
  get_command: (Yojson.Basic.t) ;
  set_command: (Yojson.Basic.t) -> unit;
  handle_command: (Yojson.Basic.t) -> unit;
  take_features: (t Feature.t) array -> unit;
  to_json: Yojson.Basic.t;
  step: t Space.t -> ((t Feature.t) * (t array) * t) list Lwt.t;
  handle_event: t Space.t -> t array -> t Feature.t -> ((t Feature.t) * (t array) * t) list Lwt.t
>

class virtual elt n (loc:HexCoordinate.t) = object

  val name = n
  val uid = UID.of_string n
  val mutable loc = loc
  val mutable env: t Environ.t = Environ.empty []
  val mutable modifier: (t Modifier.t) list = []
  val mutable command: (Yojson.Basic.t) = `Assoc []

  method get_name = name
  method get_uid = uid
  method get_env = env
  method get_loc = loc
  method set_loc l = loc <- l
  method get_command = command
  method set_command c = command <- c

  method take_features features =
    Array.iter (fun f ->
       Environ.proceed_feature f env
    ) features

  method virtual handle_command: Yojson.Basic.t -> unit
  method virtual handle_event: t Space.t -> t array -> (t Feature.t) -> ((t Feature.t) * t array * t) list Lwt.t
  method virtual to_json: Yojson.Basic.t
  method virtual step: t Space.t -> ((t Feature.t) * t array * t) list Lwt.t
end

type 'a state_trans = 'a -> t Space.t -> t -> 'a * Timer.slice

type obj_map = {
  get_obj: UID.t -> t
}

type pre_event = (t Feature.t) * (t option)

let pre_event_to_json pe =
  let f, obj_opt = pe in
  let cs = ("feature", Feature.to_json f) in
  let cs = match obj_opt with
    | None -> [cs]
    | Some obj -> cs :: [("target", `String obj#get_name)]
  in
  `Assoc cs

let compare a b = UID.compare a#get_uid b#get_uid
