open Core
open Lwt.Syntax
open UID

type dfs = Object.t -> (Feature.t * (Object.t option)) array

type npc_state = {
  description: string;
  features: (Feature.t * (Object.t option)) array;
  last: Timer.time_slice;
  tile: Object.t; (* position of the npc *)
}

let npc_state_to_json ns =
  `Assoc [
    ("description", `String ns.description)
    ; ("features", `List (List.map (fun pre -> Object.pre_event_to_json pre)
        (Array.to_list ns.features)))
    ; ("last", Timer.to_json ns.last)
    ; ("tile", `String ns.tile#get_name)
   ]

class elt n ds (tile:Object.t) = object (self)

  inherit Object.elt n

  val mutable state_trans : npc_state Object.state_trans = ds

  val mutable state = {features=[||]; last=Timer.of_int 1; tile=tile; description="诞生"}

  val mutable tile = tile

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("state", npc_state_to_json state)
    ; ("env", Environ.to_json self#get_env)
  ]

  method step universe space = begin
    (*let* spawn_events = Lwt.return @@ Environ.apply_rule self#get_env in
    let* state_commands =
    *)

    let t = Timer.play state.last in
    let* fs = if Timer.trigger t then begin
      let* _ = Logger.log "%s 完成了 %s\n" name state.description in
      let fs' = state.features in
      (* Update the current tile *)
      let ns = state_trans (state, universe, space) in
      tile <- state.tile;
      state <- ns;
      Lwt.return fs'
    end else begin
      let* _ = Logger.log "%s 正在 %s\n" name state.description in
      state <- { state with last = t};
      Lwt.return [||]
    end in
    let fs, events = Array.fold_left (fun (fs, events) (f, opt_target) ->
      match opt_target with
      | None -> (f::fs), events
      | Some obj -> fs, ((f, obj) :: events);
    ) ([], []) fs in
    self#take_features @@ Array.of_list fs;
    let* _ = Logger.log "[ local_env: %s ]\n" (Environ.dump self#get_env) in
    Lwt.return events
  end
end


class npc_attr (obj:Object.t) = object
  inherit Attribute.attr
  method name = obj#get_name
  method category = "Npc"
end

let mk_obj_attr t: Attribute.t = (new npc_attr t :> Attribute.t)

let get_npcs obj obj_map =
  let open Object in
  let attrs = Environ.filter_feature "Npc" obj#get_env in
  List.map (fun (c:Attribute.t) -> obj_map.get_obj (UID.of_string c#name)) attrs
