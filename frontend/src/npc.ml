module Tile = struct

  type tile_type = {
    base:string;
    features: string array;
  } [@@bs.deriving abstract]

  type info = {
    name:string;
    ttype:tile_type;
    hist:int;
  } [@@bs.deriving abstract]

  type t = {
    tid:string;
    center: int * int;
  }

  type tiles = {
     width: int;
     height: int;
     tiles: info array;
  }[@@bs.deriving abstract]

  let mk_tile tid center = {tid=tid; center= center}
end

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
