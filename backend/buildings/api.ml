open Lwt.Syntax
open Core

class elt n init_state ds info_builder (tile:Object.t)
  = object (self)

  inherit Object.elt n (tile#get_loc)

  val mutable state_trans : ('a Common.Api.common_state) Object.state_trans = ds

  val mutable state = Common.Api.({
    description="诞生";
    deliver=[||];
    state = init_state;
  })

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("state", Common.Api.state_to_json state info_builder)
    ; ("loc", Space.to_json self#get_loc)
    ; ("env", Environ.to_json self#get_env)
    ; ("command", Yojson.Safe.to_basic (self#get_command))
  ]

  method handle_event _ _ (* space src *) feature = begin
    match feature with
    | _ -> Lwt.return []
  end

  method handle_command _ _ = ()

  (* step universe space *)
  method step space = begin
    space.set_active (self:>Object.t);
    let loc = self#get_loc in
    let* _ = Logger.event_log "%s 完成了 %s (%d,%d) \n" name state.description (fst loc) (snd loc) in
    let* fs, events = Lwt.return @@ Array.fold_left (fun (fs, events) (f, opt_target) ->
      match opt_target with
      | None -> (f::fs), events
      | Some obj -> fs, ((Event.mk_event f (self:>Object.t) obj) :: events);
    ) ([], []) state.deliver in
    let* _ = Logger.event_log " ---- takeing features ---- " in
    let* _ = Lwt.return @@ self#take_features @@ Array.of_list fs in

    (* This is a bit clag *)
    let* _ = Logger.event_log " ---- state trans ---- " in
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
    in Lwt.return events
  end
end

let _ = Random.self_init ()

module BuildingSpawnSystem = Common.Api.SpawnSystem

let mk_building () =
  let registered_buildings = BuildingSpawnSystem.get_spawners () in
  assert (List.length registered_buildings != 0);
  let i = Random.int (List.length registered_buildings) in
  snd @@ List.nth registered_buildings i


