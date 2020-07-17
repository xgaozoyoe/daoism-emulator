module type Rectangle = sig
  val width: int
  val height: int
end

module Make (R:Rectangle) = struct

  type t = int * int

  type 'a tiles = 'a array

  type direction = | Top | TopLeft | BottomLeft | Bottom | BottomRight | TopRight

  let direction_idx = function
  | Top -> 0
  | TopLeft -> 1
  | BottomLeft -> 2
  | Bottom -> 3
  | BottomRight -> 4
  | TopRight -> 5

  let get_node x y nodes = nodes.(y * R.width + x)

  let get_index x y = (y * R.width + x)

  let set_node x y node nodes = begin
    nodes.(y * R.width + x) <- node
  end

  let from_index index =
    let x = index mod R.width in
    (x, (index - x) / R.width)

  let set_node_via_idx index node nodes = begin
    nodes.(index) <- node
  end

  (*             top
   *           /     \
   *    top_left       top_right
   *          |       |
   * bottom_left       bottom_right
   *           \     /
   *            bottom
   *)

  let top (x,y) =
    (x, y-1)

  let top_left (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    (x-1, y-1+y_offset)

  let bottom_left (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    (x-1, y+y_offset)

  let top_right (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    (x+1, y-1+y_offset)

  let bottom_right (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    (x+1, y+y_offset)

  let bottom (x,y) =
    (x, y+1)

  let siblings (x,y) =
    let y_offset = if x mod 2 = 1 then 1 else 0 in
    let top = (x, y-1) in
    let top_left = (x-1, y-1+y_offset) in
    let bottom_left = (x-1, y+y_offset) in
    let top_right = (x+1, y-1+y_offset) in
    let bottom_right = (x+1, y+y_offset) in
    let bottom = (x, y+1) in
    [|top;top_left;bottom_left;bottom;bottom_right;top_right|]

  let direction dir pos =
   match dir with
   | Top -> top pos
   | TopLeft -> top_left pos
   | TopRight -> top_right pos
   | BottomLeft -> bottom_left pos
   | BottomRight -> bottom_right pos
   | Bottom -> bottom pos

  (* get the list of position form a ray
   * in reverse order
   *)
  let ray dir pos length =
   let rec aux pos l acc = match l with
     | 0 -> acc
     | _ -> let p' = (direction dir pos) in
       aux p' (l-1) (p'::acc)
   in aux pos length [pos]

  let radius_siblings pos r =
    let split x = List.hd x, List.tl x in
    let top = List.hd @@ ray Top pos r in
    let tr, tr_ray = split @@ ray BottomRight top r in
    let br, br_ray = split @@ ray Bottom tr r in
    let b, b_ray = split @@ ray BottomLeft br r in
    let bl, bl_ray = split @@ ray TopLeft b r in
    let tl, tl_ray = split @@ ray Top bl r in
    let t, t_ray = split @@ ray TopRight tl r in
    assert (t = top);
    tr_ray @ br_ray @ b_ray @ bl_ray @ tl_ray @ t_ray

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

  let layout (x,y) =
    let lgap = 45 in
    let tgap = 52 in
    let halftgap = 26 in
    let cx = x*lgap in
    let cy = if x mod 2 = 0 then y*tgap else y*tgap + halftgap in
    (cx, cy)

end

