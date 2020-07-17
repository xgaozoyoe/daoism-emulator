let population = ref 0

let max_population = 20

let _ = Random.self_init ()

let increase_population _ =
  population := !population + 1

let try_spawn _ =
  let t = Random.int max_population in
  if t > !population then
    true
  else
    false

let decrease_population _ =
  population := !population - 1
