type coordinate = int * int

let _ = Random.init 4

let mk_cor left top = (left,top)

let mk_rand_cor _ = mk_cor (Random.int 2) (Random.int 2)

type 'a t = {
  get_path: UID.UID.t -> UID.UID.t -> UID.UID.t array;
  pick_from_coordinate: coordinate -> 'a option;
}
