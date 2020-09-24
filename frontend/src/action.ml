type loc = int * int
type command = (string -> loc -> string)
type hint = {
  ids: (string * loc * command) list;
  svg: string;
}
type state =
  | CollectCoordinate of hint
  | CollectTarget of hint
  | Idle

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

let feed_state id cor = match !state with
  | Idle -> Js.log "idle"; false
  | CollectCoordinate cands -> begin
      let r = (List.find_opt (fun (x,_,_) -> x = id) cands.ids) in
      match r with
      | Some (_, _, cmd) -> send_command (cmd id cor); true
      | _ -> false
    end
  | CollectTarget cands -> begin
      let r = (List.find_opt (fun (_,c,_) -> c = cor) cands.ids) in
      match r with
      | Some (_, _, cmd) -> send_command (cmd id cor); true
      | _ -> false
    end

let set_state s = state:=s
let reset_state s = state:=Idle

let push_state state_string cands =
  state := match state_string with
  | "coordinate" -> CollectCoordinate cands
  | "target" -> CollectTarget cands
  | _ -> Idle

let mk_command_info state_string src cand (x,y) =
  match state_string with
  | "coordinate" -> Printf.sprintf "[\"%s\", [\"Move\", [%d, %d]]]" src x y
  | "target" -> Printf.sprintf "[\"%s\", [\"Attack\", \"%s\"]]" src cand
  | _ -> "[]"
