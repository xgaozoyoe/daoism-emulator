type coordinate = int * int

let _ = Random.self_init ()
open Timer

type 'a t = {
  the_universe: unit -> 'a;
  get_path: UID.UID.t -> UID.UID.t -> UID.UID.t array;
  get_view: coordinate -> coordinate list;
  get_tile: coordinate -> 'a
  register_event: slice -> 'a -> unit;
  cancel_event: 'a -> unit;
}
