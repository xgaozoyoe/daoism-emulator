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

module Id:Element = struct
  type t = string
  let compare = String.compare
  let to_string t = t
  let of_string t = t
end

module Make (T: Element) : Full = struct

  type t = T.t list

  let compare t1 t2 =
    let rec rec_compare t1 t2 = match t1, t2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | t::tl, t'::tl' ->
      let c = T.compare t t' in
      if T.compare t t' = 0 then
        rec_compare tl tl'
      else c
    in rec_compare t1 t2

  let to_string t =
    List.fold_left (fun acc c -> acc ^ "." ^ T.to_string c)
        (T.to_string (List.hd t)) (List.tl t)

  let of_string t = List.map (fun x -> T.of_string x) (String.split_on_char '.' t)

  let get_module_name t = T.to_string (List.hd t)
  let get_middle_name t = T.to_string (List.nth t 1)

end

module UID = Make (Id)
