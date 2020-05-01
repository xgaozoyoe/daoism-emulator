open UID
type t = <
  get_name: string;
  get_env: (Feature.t * t) Environ.t;
  get_command: Attribute.t option;
  set_command: Attribute.t option -> unit;
  take_features: Feature.t array -> unit;
  step: t -> t Space.t -> (Feature.t * t) list Lwt.t
>

class virtual elt: string -> object
  val name: string
  val uid: UID.t
  val mutable env: (Feature.t * t) Environ.t
  val mutable modifier: Modifier.t list
  method get_name: string
  method take_features: Feature.t array -> unit
  method get_env: (Feature.t * t) Environ.t
  method get_command: Attribute.t option
  method set_command: Attribute.t option -> unit
  method virtual step: t -> t Space.t -> (Feature.t * t) list Lwt.t
end

type 'a state_trans = 'a * t -> 'a

type obj_map = {
  get_obj: UID.t -> t
}
