open Graph

let _ = Random.self_init ()

module TileInfo = struct

  type t = {
    x: int;
    y: int;
    index: int;
    hist: int;
    ttype: Default.tile_type;
  }

  let equal a b = (a.index = b.index)
  let hash a = a.index
  let hist a = a.hist
  let empty_info = {x=0;y=0;index=0;hist=0;ttype=Default.init_tile Default.Plain}

  let compare a b = a.index - b.index

end

module TileInfoGraph = Imperative.Graph.Concrete (TileInfo)
module HistWeight = struct
    type t = int
    type edge = (TileInfo.t * TileInfo.t)
    let compare = Int.compare
    let weight (a, b) = Int.abs @@ TileInfo.hist a - TileInfo.hist b
    let add a b = a + b
    let zero = 0
end

module HistPath = Path.Dijkstra (TileInfoGraph) (HistWeight)

module TileInfoBuilder = struct

  let build_hist ls_sand (left, top) =
    (*
    let left = left * 45 in
    let top = top * 52 in
    *)
    Array.fold_left (fun acc (pl, pt) ->
      let r = sqrt @@ float_of_int @@ (pl-left) * (pl-left) + (pt-top) * (pt-top) in
      let hist = ceil @@ (exp (-0.01 *. r)) *. (20.0 +. Float.of_int (Random.int 30)) in
      acc + (Float.to_int hist)
    ) 0 ls_sand

  let sibling_direction (from_x, from_y) (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    let top = (x, y-1) in
    let top_left = (x-1, y-1+y_offset) in
    let bottom_left = (x-1, y+y_offset) in
    let top_right = (x+1, y-1+y_offset) in
    let bottom_right = (x+1, y+y_offset) in
    let bottom = (x, y+1) in
    let direction = [|top;top_left;bottom_left;bottom;bottom_right;top_right|] in
    0

  let init_tile hist =
    let open Default in
    if hist < 5 then init_tile Default.Water
    else if hist < 15 then init_tile Default.Grassland
    else if hist < 30 then init_tile Default.Forest
    else init_tile Default.Mountain

  let build_tile_hints nsand width height =
    let ls_centers = Array.init (width * height) (fun _ -> TileInfo.empty_info) in
    let tile_graph = TileInfoGraph.create ~size:(width * height) () in
    let ls_sand = Array.init nsand (fun _ ->
     (Random.int 45 * width),
     (Random.int 52 * height)
    ) in
    let c = ref 0 in
    let get_node x y = ls_centers.(y * width + x) in
    for y=0 to height - 1 do
      for x=0 to width - 1 do
        let cx = x*45 in
        let cy = y*52 in
        let cy = if x mod 2 = 0 then cy else cy + 26 in
        let h = build_hist ls_sand (cx, cy) in
        let node = TileInfo.({
          x=cx;
          y=cy;
          index=y * width + x;
          hist=h;
          ttype=init_tile h;
        }) in
        ls_centers.(y * width + x) <- node;
        TileInfoGraph.add_vertex tile_graph ls_centers.(y * width + x);
      done
    done;
    for y=0 to height - 1 do
      for x=0 to width - 1 do
        (* anti clockwise get the adjacent tiles *)
        let c_node = get_node x y in
        if y - 1 >= 0 then begin
          (* top = (x, y - 1) *)
          let node = get_node x (y - 1) in
          TileInfoGraph.add_edge tile_graph c_node node;
          c := !c + 1
        end;
        let y_offset = if x mod 2 = 1 then 1 else 0 in
        if x - 1 >= 0 then begin
          if (y - 1 + y_offset >= 0 && y - 1 + y_offset < height ) then begin
            let node = get_node (x - 1) (y - 1 + y_offset) in
            TileInfoGraph.add_edge tile_graph c_node node;
            c := !c + 1
          end;
          if (y + y_offset < height) then begin
            let node = get_node (x - 1) (y + y_offset) in
            TileInfoGraph.add_edge tile_graph c_node node;
            c := !c + 1
          end;
        end;
      done
    done;
    let n1,_ = Array.fold_left (fun (n, hist) (c:TileInfo.t) -> if c.hist > hist then
        c, c.hist else n, hist) (get_node 0 0, 0) ls_centers in
    let n2,_ = Array.fold_left (fun (n, hist) (c:TileInfo.t) -> if c.hist < hist then
        c, c.hist else n, hist) (get_node 0 0, 1000) ls_centers in
    let edges , _ = HistPath.shortest_path tile_graph n2 n1 in
    ignore @@ List.map (fun (_, (node:TileInfo.t)) ->
      if node.ttype.base != Default.Water then begin
        let n = {node with ttype = Default.add_feature River node.ttype} in
        ls_centers.(node.index) <- n
      end
    ) edges;
    (* Add edges into graph and compute the weights *)
    ls_centers, tile_graph

end


