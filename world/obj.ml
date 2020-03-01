type t
val key: t -> string
val spawn: unit -> ref t
val add_modifier: ref t -> Motifier.t ref -> unit
val remove_modifier: ref t -> Modifier.t ref -> unit
val add_aura: ref t -> Modifier.t ref -> unit
val remove_aura: ref t -> Modifier.t ref -> unit
val settle: ref t -> unit
val handle_events: ref t -> Event.t list -> Event.t list
