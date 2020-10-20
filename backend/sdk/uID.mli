module type Element = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t
end

module type Full = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t
  val get_module_name: t -> string
  val get_middle_name: t -> string
end



module Id: Element

module Make (U:Element): Full

module UID: Full
