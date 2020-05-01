open UID
module type ATTR = sig
  type t
  val to_string: t -> string
  val category: unit -> string
end

type t = <name: string; category: string; test: t->bool; id: UID.t>

class virtual attr = object (self)
  method virtual name: string
  method virtual category: string
  method test (f: t) = f#category = self#category
  method id = UID.of_string (self#category ^ "." ^ self#name)
end


module From (ATTR:ATTR) = struct
  class ext_attr n = object
    inherit attr
    val v = n
    method name = ATTR.to_string v
    method category = ATTR.category ()
  end
end
