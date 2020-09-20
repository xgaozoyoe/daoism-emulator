type avatar_builder = (int * int) -> string
type menu = {
  avatar: avatar_builder * int; (* svg txt + height *)
  info: string Js.Dict.t;
  attributes: int Js.Dict.t;
  methods: string Js.Dict.t;
}

let mk_menu a i attr ms = {
  avatar = a;
  info = i;
  attributes = attr;
  methods = ms;
}

let reset_menu () =
  let menu_container = Document.get_by_id Document.document "menu-background" in
  let menu_ctx = Document.get_by_id Document.document "menu-ctx" in
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  Document.setInnerHTML menu_container "";
  Document.setInnerHTML menu_ctx "";
  Document.setInnerHTML menu_assist ""

let reset_assist_menu () =
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  Document.setInnerHTML menu_assist ""

let build_menu_hint svg =
  let menu_assist = Document.get_by_id Document.document "menu-assist" in
  Document.setInnerHTML menu_assist svg

let build_fix_menu _ =
  let left = 20 in
  let top = 20 in
  let top_offset = top in
  let menu_fix = Document.get_by_id Document.document "menu-fix" in
  SvgHelper.mk_rectangle_in menu_fix (30,30) (left,top_offset)

let build_menu menu candidate_builder =
  build_fix_menu ();
  reset_menu ();
  let left = 20 in
  let top = 60 in
  let padding = 5 in
  let padding_left = 10 in
  let font_height = 18 in

  (* We start with avatar *)
  let top_offset = top + padding in
  let left_offset = left + padding_left in
  let avatar_content, avatar_height = menu.avatar in
  let content = avatar_content (left_offset + avatar_height/2, top_offset + avatar_height/2) in
  let top_offset = top_offset + avatar_height + 2 * padding in

  let info, top_offset = Array.fold_left (fun (acc,top_offset) (name, info) ->
    let c = SvgHelper.mk_text "menu-item"
        (left_offset, top_offset)
        (name ^ ":" ^ info)
    in
    (acc ^ c), top_offset + font_height + padding
  ) ("", top_offset) (Js.Dict.entries menu.info) in

  let attributes, top_offset = Array.fold_left (fun (acc,top_offset) (name, info) ->
    let c = SvgHelper.mk_text "menu-item"
        (left_offset, top_offset)
        (name ^ ":" ^ (string_of_int info))
    in
    (acc ^ c), top_offset + font_height + padding
  ) ("", top_offset) (Js.Dict.entries menu.attributes) in

  let menu_container = Document.get_by_id Document.document "menu-ctx" in
  Document.setInnerHTML menu_container (content ^ info ^ attributes);

  (* Dynamic buttons with event handler *)
  let top_offset = Array.fold_left (fun top_offset (name, info) ->
    let open Action in
    let cands = candidate_builder info in
    let command () = begin
        build_menu_hint cands.svg;
        Action.push_state info cands
    end in
    SvgHelper.mk_button_in menu_container ("btn-"^name) (60, 20)
      (left_offset, top_offset)
      name command;
    top_offset + font_height + padding
  ) top_offset (Js.Dict.entries menu.methods) in

  let menu_container = Document.get_by_id Document.document "menu-background" in
  SvgHelper.mk_rectangle_in menu_container (200,top_offset) (left,top)
