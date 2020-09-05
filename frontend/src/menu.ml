let build_menu menu_info =
  let left = 20 in
  let top = 20 in
  let padding = 5 in
  let rectangle = SvgHelper.mk_rectangle (200,400) (left,top) in
  let svg, _ = List.fold_left (fun (acc,idx) (name, info) ->
    (
      Printf.sprintf "%s%s"
        acc (SvgHelper.mk_text "menu-item" (left + padding, idx * 20 + top + padding) (name ^ ":" ^ info))
    ), idx + 1
  ) ("", 1) menu_info in
  rectangle ^ svg
