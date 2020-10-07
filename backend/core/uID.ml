module type Interface = sig
  type t

  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t

end

module Id = struct
  type t = string
  let compare = String.compare
  let to_string t = t
  let of_string t = t
end

module Make (T: Interface) : Interface = struct

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

end

module UID = Make (Id)
