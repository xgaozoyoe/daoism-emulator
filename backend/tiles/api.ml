open Core
open Space
open Lwt.Syntax
open Common

module NpcAttr = Npc.Attr
module Npc = Npc.Api

(*exception SiblingException*)

class elt n ttype cor ds = object (self)

  inherit Object.elt n cor

  val mutable state_trans: tile_state Object.state_trans = ds

  val mutable state = {deliver=[||]; name="初"}

  val mutable holds: Object.t list = []

  val mutable tile_type = ttype

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("ttype", Default.to_json tile_type)
    ; ("state", tile_state_to_json state)
    ; ("loc", Space.to_json self#get_loc)
    ; ("env", Environ.to_json self#get_env)
  ]

  method handle_event _ src feature = begin
    let src = src.(0) in
    let* _ = Logger.log "[ %s handles event %s from %s]\n" (self#get_name) (Feature.to_string feature) (src#get_name) in
    match feature with
    | Hold (Object attr, _) -> begin
        let _ = match attr with
        | Enter -> src#set_loc self#get_loc; holds <- src :: holds
        | Leave -> holds <- List.filter_map (fun c ->
            if (c#get_name = src#get_name) then None else Some c
          ) holds
        | _ -> ()
        in
        Lwt.return []
      end
    | _ -> Lwt.return []
  end

  method handle_command _ = ()

  (* step universe space -> event list *)
  method step space = begin

    let* step_events = begin
      let fs, events = Array.fold_left (fun (fs, events) (f, opt_target) ->
        match opt_target with
        | None -> (f::fs), events
        | Some obj -> fs, ((f, [|(self:>Object.t)|], obj) :: events);
      ) ([], []) state.deliver in
      self#take_features @@ Array.of_list fs;
      Lwt.return events
    end in

    let* spawn_events = Lwt.return @@ List.map (fun (f,o) ->
        (f, [|(self:>Object.t)|], o)
      ) (Environ.apply_rules self#get_env) in

    (*
    let* _ = Logger.log "[ %s <%s> local_env: %s ]\n" (self#get_name) state.name (Environ.dump self#get_env) in
    let* _ = Logger.log "[ %s <%s> holds:%s ]\n" (self#get_name) state.name
         (List.fold_left (fun acc c -> acc ^ " " ^ c#get_name) "" holds) in
    *)

    (* Register step action after 5 timeslice *)
    let* s, ts = Lwt.return @@ state_trans state space (self:>Object.t) in
    state <- s;
    space.register_event ts (self:>Object.t);
    Lwt.return (spawn_events @ step_events)
  end
end

let mk_tile name typ quality cor =
  let tile = new elt name typ cor (
    Default.make_default_state quality typ (Timer.of_int 4)
  ) in
  Default.set_default_bound quality typ 10 tile;
  tile


