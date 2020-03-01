type config
type t
val get_config: t -> config
val init_world: config -> t
val register_event_handler: EventHandler.t -> unit
val run: t -> unit
val one_step: unit -> (event Event.t) list
