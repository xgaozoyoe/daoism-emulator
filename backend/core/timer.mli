type slice

val play: slice -> slice
val trigger: slice -> bool
val of_int: int -> slice
val to_string: slice -> string
val to_json: slice -> Yojson.Basic.t

module TriggerQueue : sig
  type 'a t =
    | Tail
    | Head of {
        mutable next: 'a t;
      }
    | Entity of {
        value: 'a;
        mutable wait: slice;
        mutable prev: ('a t) ref;
        mutable next: 'a t;
     }

  val mk_head: 'a t -> 'a t
  val empty: 'a t
  val register_event: slice -> 'a -> ('a t) ref -> 'a t
  val cancel_event: 'a t -> unit
  val fetch_events: ('a t) ref -> bool -> 'a list
  val dump: 'a t -> ('a -> string) -> unit Lwt.t

end
