module Dao (T: compare) (E: Event) (F:Feature) = sig

  type id

  type 'b feature
  type 'b env

  val apply: 'b env -> DaoMap.key -> 'b feature -> E.t * 'b env

  val handleEvent: 'b env -> 'a 'b E.t -> 'b env
  val pushEvent: 'b env -> 'a 'b E.t -> 'b env
  val popEvent: 'b env -> 'a 'b E.t -> 'b env

end
