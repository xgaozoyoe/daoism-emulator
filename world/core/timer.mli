type slice

val play: slice -> slice
val trigger: slice -> bool
val of_int: int -> slice
val to_string: slice -> string
val to_json: slice -> Yojson.Basic.t

module TriggerQueue : sig
  type 'a t =
    | Tail
    | Entity of {
        value: 'a;
        wait: slice;
        mutable next: 'a t;
     }

  val empty: 'a t
  val register_event: slice -> 'a -> 'a t -> 'a t
  val fetch_events: 'a list -> 'a t -> 'a list * 'a t
  val dump: 'a t -> ('a -> string) -> unit Lwt.t

end
