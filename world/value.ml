(* A bounded value watched by a few watch dogs 'a *)
open Event
module WatchDog = sig
  type t
  val uid: t -> UID.t
  val make: path -> t
  val watch: int -> int -> t Event.t
end

module Value = sig
  type t
  val get: unit -> int
  val set: int -> int
  val settle: ref t -> unit
  val add_watchdog: ref t -> WatchDog.t -> unit
  val remove_watchdog: ref t -> WatchDog.t -> unit
end




