module type Interface = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t
end

module Id: Interface

module Make (U:Interface) : Interface

module UID: Interface
