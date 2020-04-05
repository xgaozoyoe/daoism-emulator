module Dao (T: UID.Interface) (E: Event.Interface) (F:Feature.Interface) = struct

  module DaoMap = Map.Make(DaoPath)

    type 'b env = ('b feature) DaoMap.t

  let apply env feature = env

  let handleEvent env event = env
  let pushEvent env event = env
  let popEvent env event = env

end
