open UID
type t = <
  get_name: string;
  get_env: (Feature.t * t)  Environ.t;
  get_command: Attribute.t option;
  set_command: Attribute.t option -> unit;
  take_features: Feature.t array -> unit;
  step: t -> t Space.t -> (Feature.t * t) list Lwt.t
>

type 'a state_trans = 'a * t -> 'a
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

  method virtual step: t -> t Space.t -> (Feature.t * t) list Lwt.t
end

type obj_map = {
  get_obj: UID.t -> t
}
