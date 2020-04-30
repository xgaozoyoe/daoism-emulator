module ID:UID.Interface

type t = <
  get_name: string;
  get_env: (Feature.t * t) Environ.t;
  take_features: Feature.t array -> unit;
  step: t -> (Feature.t * t) list Lwt.t
>

class virtual elt: string -> object
  val name: string
  val uid: ID.t
  val mutable env: (Feature.t * t) Environ.t
  val mutable modifier: Modifier.t list
  method get_name: string
  method take_features: Feature.t array -> unit
  method get_env: (Feature.t * t) Environ.t
  method virtual step: t -> (Feature.t * t) list Lwt.t
end

type ('a, 'b) state_trans =
  'a -> 'b * (Feature.t * (t option)) array * Timer.time_slice
