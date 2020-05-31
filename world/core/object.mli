open UID
type t = <
  get_name: string;
  get_env: (Feature.t * t) Environ.t;
  get_loc: Space.coordinate;
  set_loc: Space.coordinate -> unit;
  get_command: Attribute.t option;
  set_command: Attribute.t option -> unit;
  take_features: Feature.t array -> unit;
  to_json: Yojson.Basic.t;
  step: t Space.t -> (Feature.t * t array * t) list Lwt.t;
  handle_event: t Space.t -> t array -> Feature.t -> (Feature.t * t array * t) list Lwt.t
>

class virtual elt: string -> Space.coordinate -> object
  val name: string
  val uid: UID.t
  val mutable loc: Space.coordinate
  val mutable env: (Feature.t * t) Environ.t
  val mutable modifier: Modifier.t list
  method get_name: string
  method get_loc: Space.coordinate
  method set_loc: Space.coordinate -> unit
  method take_features: Feature.t array -> unit
  method get_env: (Feature.t * t) Environ.t
  method get_command: Attribute.t option
  method set_command: Attribute.t option -> unit
  method virtual to_json: Yojson.Basic.t
  method virtual step: t Space.t -> (Feature.t * t array * t) list Lwt.t
  method virtual handle_event: t Space.t -> t array -> Feature.t -> (Feature.t * t array * t) list Lwt.t
end

(*
 * State transformation function
 * old_state -> universe -> space -> self -> new_state
 *)
type 'a state_trans = 'a -> t Space.t -> t -> 'a * Timer.slice

type obj_map = {
  get_obj: UID.t -> t
}

type pre_event = Feature.t * (t option)

val pre_event_to_json: pre_event -> Yojson.Basic.t
