type coordinate = int * int

let to_json (x,y) = `Assoc [("x", `Int x); ("y", `Int y)]

type 'a t = {
  the_universe: unit -> 'a;
  get_path: UID.UID.t -> UID.UID.t -> UID.UID.t array;
  get_view: coordinate -> 'a list;
  get_tile: coordinate -> 'a option;
  register_event: Timer.slice -> 'a -> unit;
  cancel_event: 'a -> unit;
  set_active: 'a -> unit;
}
