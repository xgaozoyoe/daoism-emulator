open Lwt.Syntax

module AttributeDamage = Core.Attribute.From(Attribute.Damage)
module DamageAttr = Attribute.Damage

open Core
open UID
open Common

class elt n ds (tile:Object.t) = object (self)

  inherit Object.elt n (tile#get_loc)

  val mutable state_trans : (npc_state) Object.state_trans = ds

  val mutable state = {
    description="诞生";
    deliver=[||];
    health = 10;
  }

  method to_json = `Assoc [
    ("name",`String self#get_name)
    ; ("state", npc_state_to_json state)
    ; ("loc", Space.to_json self#get_loc)
    ; ("env", Environ.to_json self#get_env)
  ]


  method handle_event space src feature = begin
    match feature with
    | Produce (attr, dmg) when attr#test "Damage" ->
      let src = src.(0) in
      let* _ = Lwt_io.printf "%s 受到了来自 %s 的 %d 点攻击\n"
        self#get_name src#get_name dmg in
      state <- {state with health = state.health - dmg};
      if state.health <= 0 then begin
        let loc_tile = Option.get @@ space.get_tile (self#get_loc) in
        let s, _ = Common.dead_state state loc_tile (space.the_universe ()) in
        state <- s;
        (* space register self t *)
        Lwt.return []
      end else
        Lwt.return []
    | Hold (attr, _) when attr#test "Adjacent" ->
      Lwt.return [Feature.mk_produce
        (* FIXME: src might be empty ? *)
        (new AttributeDamage.ext_attr DamageAttr.Straight) 1, [|(self :> Object.t)|], src.(0)]
    | _ -> Lwt.return []
  end

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

let get_npcs obj obj_map =
  let open Object in
  let attrs = Environ.filter_feature "Npc" obj#get_env in
  List.map (fun (c:Attribute.t) -> obj_map.get_obj (UID.of_string c#name)) attrs