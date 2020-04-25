module ID = UID.Make(UID.Id)

type t = <
  get_name: string;
  get_env: (Feature.t * t ref)  Environ.t;
  take_features: Feature.t array -> unit;
  step: t ref -> (Feature.t * t ref) list
>

type ('a, 'b) state_trans =
  'a -> 'b * (Feature.t * (t ref option)) array * Timer.time_slice

class virtual elt n = object

  val name = n
  val uid = ID.of_string n
  val mutable env: (Feature.t * t ref) Environ.t = Environ.empty
  val mutable modifier: Modifier.t list = []

  method get_name = name

  method get_env = env

  method take_features features =
    Array.iter (fun f ->
       Environ.proceed_feature f env
    ) features

  method virtual step: t ref -> (Feature.t * t ref) list
end
