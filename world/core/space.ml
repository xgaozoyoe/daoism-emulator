type coordinate = int * int

let _ = Random.self_init ()

let mk_cor left top = (left,top)

let mk_rand_cor _ = mk_cor (Random.int 16) (Random.int 8)

type 'a t = {
  get_path: UID.UID.t -> UID.UID.t -> UID.UID.t array;
  pick_from_coordinate: coordinate -> 'a option;
}
