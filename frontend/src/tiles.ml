open SvgHelper
open Global
open UID

module Tile = struct
  open Common

  type tile_type = {
    base:string;
    features: string array;
  } [@@bs.deriving abstract]


  type tile_info = {
    name:string;
    state:state;
    loc:location;
    ttype:tile_type;
    env:environ;
    holds: string array;
    inventory: (inventory Js.Nullable.t) array;
  } [@@bs.deriving abstract]

end

module WaterPath = struct

  exception UnexpectedPath

  let get_direction_point (cx, cy) =
    [|
        (cx, cy -. 26.); (cx -. 22.5, cy -. 13.)
      ; (cx -. 22.5, cy +. 13.); (cx, cy +. 26.)
      ; (cx +. 22.5, cy +. 13.); (cx +. 22.5, cy -. 13.)
    |]

  let make_path_svg (cx, cy) info = begin
    let direction_info = get_direction_point (cx, cy) in
    match info with
    | [p; q] -> begin
        let from_p = direction_info.(p) in
        let to_q = direction_info.(q) in
        mk_line from_p to_q (*cx, cy*)
      end
    | [p] -> begin
        let to_p = direction_info.(p) in
        mk_line (cx, cy) to_p
      end
    | _ -> raise UnexpectedPath
  end

end

let to_float (x,y) = (Js.Int.toFloat x, Js.Int.toFloat y)

let build_feature center f =
  let (x, y) = center in
  let feature_info = String.split_on_char '_' f in
  let feature_name = List.hd feature_info in
  let feature_info = List.map (fun c ->int_of_string c) (List.tl feature_info) in
  match feature_name with
  | "river" -> (WaterPath.make_path_svg (to_float center) feature_info)
  | _ -> Printf.sprintf "<use href='/dist/res/%s.svg#main' x='%d' y='%d' width='40' height='40'/>"
    feature_name (x-20) (y-20)

(* Make a single hexagon tile *)
let build_tile uinfo info pos radius =
  let open Tile in
  let tile_feature = info |. ttypeGet |. featuresGet in
  let feature_svg = Array.fold_left (fun svg f -> svg ^ (build_feature pos f)) "" tile_feature in
  let name = info |. nameGet in
  let typ_no = info |. ttypeGet |. baseGet in
  let style = Printf.sprintf "hex_%s" typ_no in
  let svg = SvgHelper.mk_hexagon_boundary radius style pos in
  let svg = svg ^ feature_svg in
  let svg = if (Common.has_drop (info |. inventoryGet) != 0) then
              svg ^ (SvgHelper.mk_use "item-drop" pos)
            else svg in
  svg

let build_panel uinfo info =
  let open Tile in
  let inventory = ControlPanel.mk_inventory_info (info |. inventoryGet) in
  let avatar layout = build_tile uinfo info layout 20 in
  let basic = Menu.mk_info @@
    [|
    "name", info |. nameGet;
    "type", info |. ttypeGet |. baseGet;
    "stay", Array.fold_left (fun acc c -> acc ^ ";" ^ c) "" (info |. holdsGet);
    |]
  in
  let deliver = Menu.pre_event_to_tree (info |. stateGet |. Common.deliverGet) in
  let rules = Menu.rules_to_tree @@ (info |. envGet |. Common.rulesGet) in
  Menu.mk_panel_group (avatar,40) [
    Menu.mk_panel (Button.word_avatar "B") [basic; inventory];
    Menu.mk_panel (Button.word_avatar "D") [deliver];
    Menu.mk_panel (Button.word_avatar "R") [rules]
  ]

let build_menu uinfo info =
  let open Tile in
  let menu = Menu.mk_menu [
    build_panel uinfo info
  ] in
  Menu.build_menu (info |. nameGet) menu


(*let get_tile_element idx =
  let id = "tile_" ^ (string_of_int idx) in
  Document.get_by_id Document.document id
*)

let global_tiles = ref [||]

let get_tile_info uinfo loc =
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = (uinfo |. widthGet)
    let height = (uinfo |. heightGet)
  end) in
  !global_tiles.(HexCoordinate.get_index loc)

let handle_click uinfo idx () =
  let info = !global_tiles.(idx) in
  (*let menu = Document.get_by_id Document.document "menu" in *)
  let open Tile in
  let cor = (info |. locGet |. Common.xGet), (info |. locGet |. Common.yGet) in
  let reset = if (Action.state_wait_for_coordinate ()) then begin
      if (Action.feed_state (info |. nameGet) cor ) then begin
        Menu.clear_menu_assist ();
        false
      end else true
    end else true
  in
  if reset then begin
    Action.reset_state ();
    build_menu uinfo info
  end

(* The command will show the hint and push the action state *)
let build_drop_menu src_id tinfo =
  let open Tile in
  let inventory = Array.mapi (fun i a ->
    match Js.Nullable.toOption a with
    | None -> None
    | Some equip -> Some (i, "pick", Common.get_the_entry equip)
  ) (tinfo |. inventoryGet) in
  let ms = Array.fold_left (fun acc info ->
    match info with
    | Some (i, cmd, (n, c)) ->
      let pick_arg = Action.mk_pick_arg (tinfo |. nameGet) i in
      let cmd = Action.mk_command_info cmd src_id pick_arg in
      (n, Action.mk_fixed_method cmd) :: acc
    | None -> acc
  ) [] inventory in
  Menu.mk_methods (Array.of_list ms)

let build_tiles tiles uinfo outter =
  let open Tile in
  global_tiles := tiles;
  let module HexCoordinate = HexCoordinate.Make (struct
    let width = (uinfo |. widthGet)
    let height = (uinfo |. heightGet)
  end) in
  Array.mapi (fun i info ->
    let cor = HexCoordinate.from_index i in
    let layout = HexCoordinate.layout cor in
    let item_svg = build_tile uinfo info layout 30 in
    let item = mk_group_in outter (Some (info |. nameGet)) item_svg in
    Document.add_event_listener item "click" (handle_click uinfo i)
  ) tiles

let update_tile info =
  let open Tile in
  let name = info |. nameGet in
  let middle_name = UID.get_middle_name @@ UID.of_string name in
  (!global_tiles).(int_of_string middle_name) <- info
