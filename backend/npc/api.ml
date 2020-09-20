open Lwt.Syntax
open Core
open Common

class virtual elt ?(cmd=`Assoc []) n init_state ds info_builder tile = object (self)

  inherit Object.elt ~cmd:cmd n (tile#get_loc)

  val mutable state_trans : ('a npc_state) Object.state_trans = ds

  val mutable state = {
    description="诞生";
    deliver=[||];
    state = init_state;
  }

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("state", state_to_json state info_builder)
    ; ("loc", Space.to_json self#get_loc)
    ; ("env", Environ.to_json self#get_env)
    ; ("command", Yojson.Safe.to_basic (self#get_command))
  ]

  (* step universe space *)
  method step space = begin
    space.set_active (self:>Object.t);
    let loc = self#get_loc in
    let* _ = Logger.log "%s 完成了 %s (%d,%d) \n" name state.description (fst loc) (snd loc) in
    let* fs, events = Lwt.return @@ Array.fold_left (fun (fs, events) (f, opt_target) ->
      match opt_target with
      | None -> (f::fs), events
      | Some obj -> fs, ((f, [|(self:>Object.t)|], obj) :: events);
    ) ([], []) state.deliver in
    let* _ = Logger.log " ---- takeing features ---- " in
    let* _ = Lwt.return @@ self#take_features @@ Array.of_list fs in

    (* This is a bit clag *)
    let* _ = Logger.log " ---- state trans ---- " in
    let* s, ts = Lwt.return @@ state_trans state space (self:>Object.t) in
    state <- s;
    let* _ = Logger.log "register event %s\n" (Timer.to_string ts) in
    let* _ = Lwt.return @@ space.register_event ts (self:>Object.t) in
    let* _ = Logger.log "[ local_env: %s ]\n" (Environ.dump self#get_env) in
    (* space.register_event (#self, events) *)
    Lwt.return events
  end
end
