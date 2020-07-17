module Npc = struct

  type location = {
    x:int;
    y:int;
  }[@@bs.deriving abstract]

  type state = {
    description:string;
  }[@@bs.deriving abstract]

  type t = {
    name:string;
    state:state;
    loc:location;
  }[@@bs.deriving abstract]

end


