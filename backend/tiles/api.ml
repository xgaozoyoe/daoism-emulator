open Core
open Space
open Lwt.Syntax

module Npc = Npc.Api

(*exception SiblingException*)

class elt n ttype cor ds = object (self)

  inherit Object.elt n cor

  val mutable state_trans = ds

  val mutable state = Common.Api.({deliver=[||]; description="初"; state=()})

  val mutable holds: Object.t list = []

  val mutable tile_type = ttype

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("ttype", Default.to_json tile_type)
    ; ("state", Common.Api.state_to_json state (fun _ -> `String "" ))
    ; ("loc", Space.to_json self#get_loc)
    ; ("holds", `List (List.map (fun x -> `String (x#get_name)) holds))
    ; ("env", Environ.to_json self#get_env)
    ; ("inventory", Object.inventory_to_json self#get_inventory)
  ]

  method handle_event space src feature = begin
    let* _ = Lwt_io.printf "[ %s handles event %s from %s]\n" (self#get_name) (Feature.to_string feature) (src#get_name) in
    let* evts = match feature with
    | Hold (Notice attr, _) -> begin
        let evts = match attr with
        | Enter -> begin
            let leave_tile = Option.get @@ space.get_tile (src#get_loc) in
            src#set_loc self#get_loc;
            holds <- List.filter_map (fun c ->
              if (c#get_name = src#get_name) then None else Some c
            ) holds;
            holds <- src :: holds;
            let feature = Feature.mk_hold (Common.Api.CommonState.mk_notice_attr Attribute.Api.Leave) 1 in
            [Event.mk_event feature src leave_tile]
          end
        | Leave ->
          holds <- List.filter_map (fun c ->
            if (c#get_name = src#get_name) then None else Some c
          ) holds;
          []
        | _ -> []
        in
        Lwt.return evts
      end
    | Hold (Equipment att, k) ->
      let inventory = self#get_inventory in
      let _ = try
          for i=0 to (Array.length inventory) - 1 do
            match inventory.(i) with
            | None -> begin
                inventory.(i) <- Some (Equipment att, k);
                raise Not_found
              end
            | Some _ -> ()
          done
        with _ -> ()
      in Lwt.return []
    | _ -> Lwt.return []
    in
    let* _ = Lwt_io.printf "[ %s holds %d]\n" (self#get_name) (List.length holds) in
    let* _ = Lwt_list.iter_s (fun e -> Lwt_io.printf ">> holds: %s\n" e#get_name) holds in
    let (x,y) = src#get_loc in
    let* _ = Lwt_io.printf "[ %s loc %d, %d]\n" (self#get_name) x y in
    Lwt.return evts
  end

  method handle_command _ _ = ()

  (* step universe space -> event list *)
  method step space = begin

    let* step_events = begin
      let fs, events = Array.fold_left (fun (fs, events) (f, opt_target) ->
        match opt_target with
        | None -> (f::fs), events
        | Some obj -> fs, ((Event.mk_event f (self:>Object.t) obj) :: events);
      ) ([], []) state.deliver in
      self#take_features @@ Array.of_list fs;
      Lwt.return events
    end in

    let* spawn_events = Lwt.return @@ List.map (fun (f,o) ->
        (Event.mk_event f (self:>Object.t) o)
      ) (Environ.apply_rules self#get_env) in

    (*
    let* _ = Logger.event_log "[ %s <%s> local_env: %s ]\n" (self#get_name) state.name (Environ.dump self#get_env) in
    let* _ = Logger.event_log "[ %s <%s> holds:%s ]\n" (self#get_name) state.name
         (List.fold_left (fun acc c -> acc ^ " " ^ c#get_name) "" holds) in
    *)

    (* Register step action after 5 timeslice *)
    let* s, ts = Lwt.return @@ state_trans state space (self:>Object.t) in
    state <- s;
    let* _ = match ts with
    | Some t -> begin
        let* _ = Logger.event_log "register event %s\n" (Timer.to_string t) in
        let* _ = Lwt.return @@ space.register_event t (self:>Object.t) in
        let* _ = Logger.event_log "[ local_env: %s ]\n" (Environ.dump self#get_env) in
        Lwt.return ()
      end
    | None -> Lwt.return ()
    in Lwt.return (spawn_events @ step_events)
  end
end

let mk_tile name typ quality cor =
  let tile = new elt name typ cor (
    Default.make_default_state quality typ (Timer.of_int 4)
  ) in
  Default.set_default_bound quality typ 10 tile;
  tile


