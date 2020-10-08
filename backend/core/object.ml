open Sdk.UID
open Utils
module Attribute = Attribute.Api
type t = <
  get_name: string;
  get_uid: UID.t;
  get_loc: HexCoordinate.t;
  set_loc: HexCoordinate.t -> unit;
  get_env: t Environ.t;
  get_inventory: (t Environ.elt option) array;
  get_command: (Yojson.Safe.t) ;
  set_command: (Yojson.Safe.t) -> unit;
  set_inventory: (t Environ.elt option) array -> unit;
  set_event_entry: t Timer.TriggerQueue.t -> unit;
  get_event_entry: t Timer.TriggerQueue.t;
  handle_command: (t Space.t) -> (Yojson.Safe.t) -> unit;
  take_features: (t Feature.t) array -> unit;
  to_json: Yojson.Basic.t;
  step: t Space.t -> (t Event.t) list Lwt.t;
  handle_event: t Space.t -> t -> t Feature.t -> (t Event.t) list Lwt.t
>

class virtual elt ?(cmd=`Assoc []) n (loc:HexCoordinate.t) = object

  val name = n
  val uid = UID.of_string n
  val mutable loc = loc
  val mutable env: t Environ.t = Environ.empty []
  val mutable inventory: (t Environ.elt option) array = [|None; None; None; None|]
  val mutable modifier: (t Modifier.t) list = []
  val mutable command: (Yojson.Safe.t) = cmd
  val mutable event_entry: (t Timer.TriggerQueue.t) = Timer.TriggerQueue.empty

  method get_name = name
  method get_uid = uid
  method get_env = env
  method get_loc = loc
  method set_loc l = loc <- l
  method get_command = command
  method get_inventory = inventory
  method set_inventory c = inventory <- c
  method set_command c = command <- c
  method get_event_entry = event_entry
  method set_event_entry e = event_entry <- e

  method take_features features =
    Array.iter (fun f ->
       Environ.proceed_feature f env
    ) features

  method virtual handle_command: (t Space.t) -> Yojson.Safe.t -> unit
  method virtual handle_event: t Space.t -> t -> (t Feature.t) -> (t Event.t) list Lwt.t
  method virtual to_json: Yojson.Basic.t
  method virtual step: t Space.t -> (t Event.t) list Lwt.t
end

type 'a state_trans = 'a -> t Space.t -> t -> 'a * (Timer.slice option)

type obj_map = {
  get_obj: UID.t -> t
}

type pre_event = (t Feature.t) * (t option)

let pre_event_to_json pe =
  let f, obj_opt = pe in
  let cs = ("feature", Feature.to_json f) in
  let cs = match obj_opt with
    | None -> cs :: [("target", `String "self")]
    | Some obj -> cs :: [("target", `String obj#get_name)]
  in
  `Assoc cs

let compare a b = UID.compare a#get_uid b#get_uid

let equip_item obj idx =
  let invent = obj#get_inventory in
  match invent.(idx) with
  | None -> ()
  | Some attr -> begin
      let attr = Environ.replace_feature attr (obj#get_env) in
      invent.(idx) <- attr
    end

exception InventorySlotNotEmpty
let add_to_inventory obj idx attr =
  let invent = obj#get_inventory in
  match invent.(idx) with
  | None -> invent.(idx) <- Some attr
  | Some _ -> raise InventorySlotNotEmpty


let inventory_to_json ivt =
  `List (Array.to_list @@ Array.map (fun c ->
     match c with
     | None -> `Null
     | Some attr -> Environ.elt_to_json attr
     ) ivt)
