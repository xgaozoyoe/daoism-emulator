type 'a universe = {
  spawn: 'a ref -> unit;
}

(*
type attr_npc =
  | Apprentice

class attrNPC attr_npc = object (self)
  method name = match attr_npc with
    | Apprentice -> "Apprentice"
  method category = "Npc"
  method test (f:Attribute.t) = f#category = self#category
end
*)


