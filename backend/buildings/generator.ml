module TileInfo = Tiles.Generator.TileInfo
exception Break of bool
let _ = Temple.init ()
let _ = Village.init ()
let generate_building node siblings =
  let open TileInfo in
  try begin
    match node.ttype.features with
    | [] -> begin
      let _ = List.fold_left (fun _ n ->
        match n.ttype.base with
        | Tiles.Default.Water -> raise (Break false)
        | _ -> if n.hist > node.hist
          then raise (Break false)
          else true
      ) true siblings in
      Option.some @@
        Attribute.Api.Spawn (Attribute.Spawn.Building (Api.mk_building ()))
      end
    | _ -> raise (Break false)
  end with
  | Break _ -> None
