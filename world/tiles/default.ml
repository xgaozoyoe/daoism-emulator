module AttributeWuXing = Core.Attribute.From(Attribute.WuXing)
let make_state _ timeslice = fun _ ->
   let open Core in
   let open AttributeWuXing in
   let f = Feature.mk_produce (new ext_attr Jing) 1 in
    ("贫瘠", [| (f, None) |], timeslice)
