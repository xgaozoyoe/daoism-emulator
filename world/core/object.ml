module ID = UID.Make(UID.Id)

type t = <
  get_name: string;
  get_env: (Feature.t * t)  Environ.t;
  take_features: Feature.t array -> unit;
  step: t -> (Feature.t * t) list
>

type ('a, 'b) state_trans =
  'a -> 'b * (Feature.t * (t option)) array * Timer.time_slice

class virtual elt n = object

  val name = n
  val uid = ID.of_string n
  val mutable env: (Feature.t * t) Environ.t = Environ.empty []
  val mutable modifier: Modifier.t list = []

  method get_name = name

  method get_env = env

  method take_features features =
    Array.iter (fun f ->
       Environ.proceed_feature f env
    ) features

  method virtual step: t -> (Feature.t * t) list
end
