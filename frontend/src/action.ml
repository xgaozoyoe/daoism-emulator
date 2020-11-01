type loc = int * int
type command = (string -> loc -> string)

type state =
  | CollectTarget of ((string -> string) * (string list))
  | Idle

let mk_collect_state (cmd, ids) = CollectTarget (cmd, ids)

type action_method =
| Fixed of string
| Transitive of string * state

let mk_fixed_method str = Fixed str
let mk_transitive_method (svg, str) = Transitive (svg, str)

type method_info = (string * string)
type hint = string * action_method
type hint_builder = method_info -> hint

let state = ref Idle

let to_string ids = List.fold_left (fun acc c -> acc ^ "; " ^ c) "" ids

let cmd_fetch_data = "[ \"FetchData\" ]"
let cmd_command_data command =
    Printf.sprintf "[ \"Command\", %s]" command

let send_fetch_data _ =
  Connection.send_command Connection.ws cmd_fetch_data

let send_command cmd =
  let command = cmd_command_data cmd in
  Js.log command;
  Connection.send_command Connection.ws command

let state_wait_for_coordinate () = match !state with
  | CollectTarget _ -> true
  | _ -> false

let mk_target_arg src = Printf.sprintf "\"%s\"" src
let mk_pick_arg src idx = Printf.sprintf "[\"%s\", %d]" src idx

let mk_command_info command_hint src arg_string =
  Printf.sprintf "[\"%s\", [\"%s\", %s]]" src command_hint arg_string

let feed_state id _ = match !state with
  | Idle -> Js.log "idle"; false
  | CollectTarget (command, cands) -> begin
      let r = (List.find_opt (fun c -> c = id) cands) in
      match r with
      | Some _ -> begin
          let cmd = command id in
          send_command cmd;
          true
        end
      | _ -> false
    end

let set_state s = state:=s
let reset_state s = state:=Idle
let push_state s = (state := s)
