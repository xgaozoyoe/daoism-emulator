open UID
open Utils
module Attribute = Attribute.Api
type t = <
  get_name: string;
  get_uid: UID.t;
  get_env: t Environ.t;
  get_loc: HexCoordinate.t;
  get_inventory: (t Environ.elt option) array;
  set_loc: HexCoordinate.t -> unit;
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

class virtual elt: ?cmd:Yojson.Safe.t -> string -> HexCoordinate.t -> object
  val name: string
  val uid: UID.t
  val mutable loc: HexCoordinate.t
  val mutable env: t Environ.t
  val mutable modifier: t Modifier.t list
  method get_name: string
  method get_uid: UID.t
  method get_loc: HexCoordinate.t
  method set_loc: HexCoordinate.t -> unit
  method take_features: (t Feature.t) array -> unit
  method get_env: t Environ.t
  method get_command: (Yojson.Safe.t)
  method get_inventory: (t Environ.elt option) array
  method set_command: (Yojson.Safe.t) -> unit
  method set_inventory: (t Environ.elt option) array -> unit
  method set_event_entry: t Timer.TriggerQueue.t -> unit
  method get_event_entry: t Timer.TriggerQueue.t
  method virtual handle_command: t Space.t -> Yojson.Safe.t -> unit
  method virtual to_json: Yojson.Basic.t
  method virtual step: t Space.t -> (t Event.t) list Lwt.t
  method virtual handle_event: t Space.t -> t -> t Feature.t -> (t Event.t) list Lwt.t
end

(*
 * State transformation function
 * old_state -> universe -> space -> self -> new_state
 *)
type 'a state_trans = 'a -> t Space.t -> t -> 'a * (Timer.slice option)

type obj_map = {
  get_obj: UID.t -> t
}

type pre_event = (t Feature.t) * (t option)

val pre_event_to_json: pre_event -> Yojson.Basic.t

val compare: t -> t -> int
val equip_item: t -> int -> unit
