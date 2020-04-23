module ID:UID.Interface

type t = <
  get_name: string;
  get_env: Environ.t;
  take_features: Feature.t array -> unit;
  step: (t ref) -> (Feature.t * t ref) list
>

class virtual elt: string -> object
  val name: string
  val uid: ID.t
  val mutable env: Environ.t
  val mutable modifier: Modifier.t list
  method get_name: string
  method take_features: Feature.t array -> unit
  method get_env: Environ.t
  method virtual step: (t ref) -> (Feature.t * t ref) list
end

type ('a, 'b) state_trans =
  'a -> 'b * (Feature.t * (t ref option)) array * Timer.time_slice
