open SvgHelper
let word_avatar w = (fun (cl, ct) ->
  let c = SvgHelper.mk_text "menu-item" (cl, ct + 10) w in
  c ^ (mk_rectangle "menu-rect" (30,30) (cl-15, ct-15))
), 30
