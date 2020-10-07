open Common
type avatar_builder = (int * int) -> string

type menu = {
  avatar: avatar_builder * int; (* svg txt + height *)
  info: string Js.Dict.t;
  attributes: int Js.Dict.t;
  methods: string Js.Dict.t;
  rules: rule array;
  deliver: pre_event array;
}

type layout_info = {
  left: int;
  top: int;
  padding: int;
  font_size: int;
}

let mk_menu a i attr ms rs dlvr = {
  avatar = a;
  info = i;
  attributes = attr;
  methods = ms;
  rules = rs;
  deliver = dlvr;
}

type ('a, 'b) field_info = {
  key_func: 'a -> string;
  dict_func: 'a -> 'b Js.Dict.t;
  to_string: 'b -> string;
}

let rules_fields = {
  key_func = resultGet;
  dict_func = requireGet;
  to_string = string_of_int;
}

let event_fields = {
  key_func = targetGet;
  dict_func = featureGet;
  to_string = (fun x -> x)
}

let current_focus_id : (string option) ref = ref None

let reset_menu () =
  current_focus_id := None;
  let menu_container = Document.get_by_id Document.document "menu-background" in
  let menu_ctx = Document.get_by_id Document.document "menu-ctx" in
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  let menu_fix = Document.get_by_id Document.document "menu-fix" in
  Document.setInnerHTML menu_container "";
  Document.setInnerHTML menu_ctx "";
  Document.setInnerHTML menu_assist "";
  Document.setInnerHTML menu_fix ""

let reset_assist_menu () =
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  Document.setInnerHTML menu_assist ""

let build_menu_hint svg =
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  Document.setInnerHTML menu_assist svg

let content_start = {left=20; top=20; padding=5; font_size=18}

let show_panel cb () =
  let menu_container = Document.get_by_id Document.document "menu-ctx" in
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  let menu_bg = Document.get_by_id Document.document "menu-background" in
  Document.setInnerHTML menu_container "";
  Document.setInnerHTML menu_assist "";
  Document.setInnerHTML menu_bg "";
  cb menu_container

let build_panel avatar (left, top) cb =
  let menu_fix = Document.get_by_id Document.document "menu-fix" in

  (* Adding panel tab into menu-fix *)
  let tab = Document.createElementSVG Document.document "g" in
  Document.appendChild menu_fix tab;
  let avatar_content, avatar_height = avatar in
  let top_center = top + avatar_height/2 in
  let left_center = left + avatar_height/2 in
  let content = avatar_content (left_center, top_center) in
  Document.setInnerHTML tab content;
  let _ = SvgHelper.mk_rectangle_in tab "menu-rect" (40,40)
    (left,top) in
  Document.add_event_listener tab "click" (show_panel cb)

let panel_start = {left=20; top=70; padding=5; font_size=18}

(* show diction attributes *)
let build_dict_attrs dict to_string pinfo outter =
  let g = Document.createElementSVG Document.document "g" in
  let left = pinfo.left + pinfo.padding in
  let info, top_offset = Array.fold_left (fun (acc,top_offset) (name, info) ->
    let c = SvgHelper.mk_text "menu-item"
        (left, top_offset)
        (name ^ ":" ^ (to_string info))
    in
    (acc ^ c), top_offset + pinfo.font_size + pinfo.padding
  ) ("", pinfo.top + pinfo.padding + pinfo.font_size) (Js.Dict.entries dict) in
  Document.setInnerHTML g info;
  Document.appendChild outter g;
  top_offset - pinfo.font_size + pinfo.padding

let build_info_tree rules finfo pinfo outter =
  let g = Document.createElementSVG Document.document "g" in
  let left_offset = pinfo.left + pinfo.padding in
  Js.log rules;
  Js.log "Build info tree!";
  let (info, top_offset) = Array.fold_left (fun (acc, top) rule ->
    let top = top + pinfo.padding + pinfo.font_size in
    let dict = rule |. finfo.dict_func in
    let label = rule |. finfo.key_func in
    let c = SvgHelper.mk_text "menu-item"
        (left_offset, top)
        label
    in
    let acc = acc ^ c in
    let top_offset = top + pinfo.padding + pinfo.font_size in
    let left_offset = left_offset + pinfo.padding * 2 in
    Array.fold_left (fun (acc, top) (name,value) ->
      let c = SvgHelper.mk_text "menu-item"
        (left_offset, top)
        (name ^ ":" ^ finfo.to_string value)
      in
      (acc ^ c), top_offset + pinfo.font_size + pinfo.padding
    ) (acc, top_offset) (Js.Dict.entries dict)
  )  ("", pinfo.top) rules
  in
  Document.setInnerHTML g info;
  Document.appendChild outter g;
  top_offset

let build_menu_methods methods hint_builder pinfo outter =
  (* Dynamic buttons with event handler *)
  let top_offset = pinfo.top in
  let left_offset = pinfo.left + pinfo.padding in
  let left_boundary = pinfo.left + 200 in
  let button_width = 30 in
  let (_, top_offset) = Array.fold_left (fun (left, top) (name, info) ->
    let open Action in
    let cands = hint_builder info in
    let command () = begin
        build_menu_hint cands.svg;
        Action.push_state info cands
    end in
    let _ = SvgHelper.mk_button_in outter ("btn-"^name) (button_width, button_width)
      (left, top) name command in
    let (left, top) =
      if (left + pinfo.padding * 2 + button_width > left_boundary) then
        (left + pinfo.padding + button_width , top)
      else
        (left + pinfo.padding + button_width, top)
        (*left_offset, top + button_width + pinfo.padding*)
    in
    (left, top)
  ) (left_offset, top_offset) (Js.Dict.entries methods) in
  top_offset

let is_current_focus focus_id = (!current_focus_id = Some focus_id)
let current_focus _ =
  match !current_focus_id with
  | None -> "None"
  | Some id -> id

let build_menu focus_id menu hint_builder =

  reset_menu ();
  current_focus_id := Some focus_id;

  let pinfo = panel_start in

  (* Panel one, contains base info and methods *)
  let base_cb outter = begin
    let top_offset = build_dict_attrs menu.info (fun x -> x)
        panel_start outter in
    let top_offset = build_menu_methods menu.methods hint_builder
        {pinfo with top = top_offset} outter in
    let menu_container = Document.get_by_id Document.document "menu-background" in
    SvgHelper.mk_rectangle_in menu_container "menu" (200,top_offset)
        (panel_start.left, panel_start.top)
  end in
  build_panel menu.avatar (20,20) base_cb;

  (* Panel two, contains attributes info *)
  let attributes_cb outter = begin
    let top_offset = build_dict_attrs menu.attributes (fun x -> string_of_int x)
        panel_start outter in
    let menu_container = Document.get_by_id Document.document "menu-background" in
    SvgHelper.mk_rectangle_in menu_container "menu" (200,top_offset)
        (panel_start.left, panel_start.top)
  end in

  (* Panel three, contains rules info *)
  let rules_cb outter = begin
    let top_offset = build_info_tree menu.rules rules_fields
        panel_start outter in
    let menu_container = Document.get_by_id Document.document "menu-background" in
    SvgHelper.mk_rectangle_in menu_container "menu" (200,top_offset)
        (panel_start.left, panel_start.top)
  end in

  (* Panel three, contains rules info *)
  let deliver_cb outter = begin
    let top_offset = build_info_tree menu.deliver event_fields
        panel_start outter in
    let menu_container = Document.get_by_id Document.document "menu-background" in
    SvgHelper.mk_rectangle_in menu_container "menu" (200,top_offset)
        (panel_start.left, panel_start.top)
  end in

  let attr_avatar = (fun (cl, ct) -> SvgHelper.mk_text "menu-item" (cl, ct + 10) "B"), 30 in
  build_panel attr_avatar (70,20) attributes_cb;

  let dao_avatar = (fun (cl, ct) -> SvgHelper.mk_text "menu-item" (cl, ct + 10) "D"), 30 in
  build_panel dao_avatar (120,20) deliver_cb;

  let dao_avatar = (fun (cl, ct) -> SvgHelper.mk_text "menu-item" (cl, ct + 10) "R"), 30 in
  build_panel dao_avatar (170,20) rules_cb;

  ignore @@ show_panel base_cb ();

  ()
