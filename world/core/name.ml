let random_name _ = "Random Name"
let name_map = Hashtbl.create 10
let gen_name category =
  let name_count = Hashtbl.find_opt name_map category in
  match name_count with
  | None ->
    Hashtbl.add name_map category 1;
    Printf.sprintf "%s.%d" category 0
  | Some c ->
    Hashtbl.add name_map category (c+1);
    Printf.sprintf "%s.%d" category c
