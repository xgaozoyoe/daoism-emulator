open Graph

let _ = Random.self_init ()

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

module TileInfo = struct

  type t = {
    cor: (int * int);
    cx: int;
    cy: int;
    index: int;
    hist: int;
    ttype: Default.tile_type;
  }

  let equal a b = (a.index = b.index)
  let hash a = a.index
  let hist a = a.hist
  let empty_info = {cor=(0,0);cx=0;cy=0;index=0;hist=0;ttype=Default.init_tile Default.Plain}

  let compare a b = a.index - b.index

end

(* Edges that uses delta as its weight *)
module HistWeight = struct
    type t = int
    type edge = (TileInfo.t * TileInfo.t)
    let compare = Int.compare
    let weight (a, b) = Int.abs @@ TileInfo.hist a - TileInfo.hist b
    let add a b = a + b
    let zero = 0
end

module type Rectangle = sig
  val width: int
  val height: int
end

module HexCoordinate (R:Rectangle) = struct

  type 'a tiles = 'a array

  let get_node x y nodes = nodes.(y * R.width + x)

  let get_index x y = (y * R.width + x)

  let set_node x y node nodes = begin
    nodes.(y * R.width + x) <- node
  end

  let set_node_via_idx index node nodes = begin
    nodes.(index) <- node
  end

  let siblings (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    let top = (x, y-1) in
    let top_left = (x-1, y-1+y_offset) in
    let bottom_left = (x-1, y+y_offset) in
    let top_right = (x+1, y-1+y_offset) in
    let bottom_right = (x+1, y+y_offset) in
    let bottom = (x, y+1) in
    [|top;top_left;bottom_left;bottom;bottom_right;top_right|]

  let sibling_fold target func init nodes =
    let node = get_node (fst target) (snd target) nodes in
    let direction = siblings target in
    let acc, _ = Array.fold_left (fun (acc,idx) (x,y) ->
      if (x >=0 && y>=0 && x<R.width && y<R.height) then
        func acc node (get_node x y nodes) idx, idx + 1
      else (acc, idx+1)
    ) (init,0) direction in
    acc

  let fold_with_kernel func init nodes =
    for y=0 to R.height - 1 do
      for x=0 to R.width - 1 do
        sibling_fold (x,y) func init nodes
      done
    done
end

module TileGraph (R:Rectangle) (C:Comparable) = struct

  module HexCoordinate = HexCoordinate (R)
  open HexCoordinate

  include Imperative.Graph.Concrete (C)

  let init_graph nodes graph vgen egen = begin

    for y=0 to R.height - 1 do
      for x=0 to R.width - 1 do
        let node = vgen x y in
        set_node x y node nodes;
        add_vertex graph node
      done
    done;

    for y=0 to R.height - 1 do
      for x=0 to R.width - 1 do
        (* anti clockwise get the adjacent tiles *)
        let c_node = get_node x y nodes in
        if y - 1 >= 0 then begin
          let node = get_node x (y - 1) nodes in
          add_edge graph c_node node
        end;
        let y_offset = if x mod 2 = 1 then 1 else 0 in
        if x - 1 >= 0 then begin
          if (y - 1 + y_offset >= 0 && y - 1 + y_offset < R.height )
          then begin
            let node = get_node (x - 1) (y - 1 + y_offset) nodes in
            add_edge graph c_node node
          end;
          if (y + y_offset < R.height) then begin
            let node = get_node (x - 1) (y + y_offset) nodes in
            if egen c_node node then
              add_edge graph c_node node
            else ()
          end;
        end;
      done
    done
  end
end

module TileInfoBuilder (R: Rectangle)= struct

  module TileInfoGraph = TileGraph (R) (TileInfo)

  module HistPath = Path.Dijkstra (TileInfoGraph) (HistWeight)

  module Coordinate = HexCoordinate (R)

  let nodes = Array.init (R.width * R.height) (fun _ -> TileInfo.empty_info)

  let tile_graph = TileInfoGraph.create ~size:(R.width * R.height) ()

  let build_hist ls_sand (left, top) =
    Array.fold_left (fun acc (pl, pt) ->
      let r = sqrt @@ float_of_int @@ (pl-left) * (pl-left) + (pt-top) * (pt-top) in
      let hist = ceil @@ (exp (-0.01 *. r)) *. (20.0 +. Float.of_int (Random.int 30)) in
      acc + (Float.to_int hist)
    ) 0 ls_sand


  let init_tile hist =
    let open Default in
    if hist < 5 then init_tile Default.Water
    else if hist < 15 then init_tile Default.Grassland
    else if hist < 30 then init_tile Default.Forest
    else init_tile Default.Mountain



  let build_rivers _ = begin

    let open TileInfo in

    (* Generate rivers *)
    let n1,_ = Array.fold_left (fun (n, hist) (c:TileInfo.t) -> if c.hist > hist then
        c, c.hist else n, hist) (Coordinate.get_node 0 0 nodes, 0) nodes in

    let n2,_ = Array.fold_left (fun (n, hist) (c:TileInfo.t) -> if c.hist < hist then
        c, c.hist else n, hist) (Coordinate.get_node 0 0 nodes, 1000) nodes in

    let edges , _ = HistPath.shortest_path tile_graph n2 n1 in

    let water_sibling = Array.init (R.width * R.height) (fun _ -> None) in

    (* Mark the river node *)
    ignore @@ List.map (fun (_, node) ->
      water_sibling.(node.index) <- Some []
    ) edges;

    Coordinate.fold_with_kernel (fun _ node sibling idx ->
      match water_sibling.(node.index) with
      | Some ls -> if water_sibling.(sibling.index) != None then
        water_sibling.(node.index) <- Some (idx :: ls)
      | _ -> ()
    ) () nodes;

    (* Add features to rivers *)
    ignore @@ List.map (fun (_, (node:TileInfo.t)) ->
      if node.ttype.base != Default.Water then begin
        let feature = Default.add_feature (River (Option.get water_sibling.(node.index))) node.ttype in
        Coordinate.set_node_via_idx node.index {node with ttype = feature} nodes
      end
    ) edges;
  end

  let init_graph nsand = begin

    (* Initialize hist generator *)
    let ls_sand = Array.init nsand (fun _ ->
     (Random.int 45 * R.width),
     (Random.int 52 * R.height)
    ) in

    (* Initialize vetex *)
    let vetex_gen x y =
      let cx = x*45 in
      let cy = y*52 in
      let cy = if x mod 2 = 0 then cy else cy + 26 in
      let h = build_hist ls_sand (cx, cy) in
      TileInfo.({
        cor=(x,y);
        cx=cx;
        cy=cy;
        index= Coordinate.get_index x y;
        hist=h;
        ttype=init_tile h;
      })
    in

    let edge_gen _ _ = true in

    (*
     * Initialze the graph based on vetex_gen which compute the vetex
     * and edge_gen which decides connectivity
     *)

    TileInfoGraph.init_graph nodes tile_graph vetex_gen edge_gen;

  end



end


