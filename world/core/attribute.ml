module type ATTR = sig
  type t
  val to_string: t -> string
  val category: unit -> string
end

module ID = UID.Make(UID.Id)
type t = <name: string; category: string; test: t->bool; id: ID.t>

class virtual attr = object (self)
  method virtual name: string
  method virtual category: string
  method test (f: t) = f#category = self#category
  method id = ID.of_string (self#category ^ "." ^ self#name)
end

module From (ATTR:ATTR) = struct
  class ext_attr n = object
    inherit attr
    val v = n
    method name = ATTR.to_string v
    method category = ATTR.category ()
  end
end
