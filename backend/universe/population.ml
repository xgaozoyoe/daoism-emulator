let population = ref 0

let max_population = 3

let _ = Random.self_init ()

let increase_population _ =
  population := !population + 1

let try_spawn _ =
  if max_population > !population then
    true
  else
    false

let decrease_population _ =
  population := !population - 1
