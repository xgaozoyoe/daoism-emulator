type coordinate = int * int

type 'a t = {
  the_universe: unit -> 'a;
  get_path: UID.UID.t -> UID.UID.t -> UID.UID.t array;
  get_view: coordinate -> 'a list;
  get_tile: coordinate -> 'a option;
  register_event: Timer.slice -> 'a -> unit;
  cancel_event: 'a -> unit;
}
