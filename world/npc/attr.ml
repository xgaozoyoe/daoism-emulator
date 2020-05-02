open Core
class npc_attr (obj:Object.t) = object
  inherit Attribute.attr
  method name = obj#get_name
  method category = "Npc"
end

let mk_obj_attr t: Attribute.t = (new npc_attr t :> Attribute.t)

class tile_attr = object
  inherit Attribute.attr
  method name = "meet"
  method category = "Adjacent"
end

let mk_tile_attr _: Attribute.t = (new tile_attr :> Attribute.t)

class status_attr name = object
  inherit Attribute.attr
  method name = name
  method category = "Status"
end

let mk_status_attr t: Attribute.t = (new status_attr t :> Attribute.t)





