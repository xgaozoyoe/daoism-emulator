(* Total amount of attributes to create apprentice *)

let total_wuxing_amount q =
  let open Core.Quality in
  match q with
  | Unique -> 200
  | Rare -> 150
  | Elite -> 120
  | Normal -> 100
  | Antique -> Random.int 200
