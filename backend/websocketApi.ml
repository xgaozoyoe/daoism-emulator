open Lwt.Syntax
open Websocket
open Websocket_lwt_unix

exception ConnectionLost of string

module ResponseBuilder = struct
  let build_response category json = `Assoc [
    ("method",`String category);
    ("data", json)
  ]
end

module UpdateDispatcher = struct
  let connection_table = Hashtbl.create 10
  let table_lock = Lwt_mutex.create ()
  let add_sender sender id =
    Lwt_mutex.with_lock table_lock @@ (fun _ ->
      Lwt.return @@ Hashtbl.add connection_table id sender
    )
  let remove_sender id =
    Lwt_mutex.with_lock table_lock @@ (fun _ ->
      Lwt.return @@ Hashtbl.remove connection_table id
    )
  (* FIXME: concurrency problem here
   * client might disconnect at any time
   *)
  let broadcast msg_json =
    let msg = ResponseBuilder.build_response "update" msg_json in
    let msg = Yojson.Basic.to_string msg in
    let seq = List.of_seq @@ Hashtbl.to_seq connection_table in
    Lwt_list.iter_s (fun (_, sender) ->
      sender @@ Frame.create ~content:msg ()
    ) seq
end

module GlobalData = struct
  type resp_data = {
    mutable resp: Yojson.Basic.t;
  }

  let resp_data = {
    resp = `String "Welcome";
  }

  let resp_lock = Lwt_mutex.create ()

  let update_data data =
    Lwt_mutex.with_lock resp_lock @@ (fun _ ->
      resp_data.resp <- data;
      Lwt.return ()
    )

  let send_data send =
    Lwt_mutex.with_lock resp_lock @@ (fun _ ->
      let data = ResponseBuilder.build_response "global" resp_data.resp in
      let buf = Yojson.Basic.to_string data in
      send @@ Frame.create ~content:buf ()
    )
end

let section = Lwt_log.Section.make "websocket"

let handler cb id client =
  incr id;
  let id = !id in
  let send = Connected_client.send client in
  let* _ = UpdateDispatcher.add_sender send id in
  Lwt_log.ign_info_f ~section "New connection (id = %d)" id;

  let rec recv_forever () =
    let open Frame in
    let react fr =
      let* _ = Lwt_log.debug_f ~section "<- %s" (Frame.show fr) in
      match fr.opcode with
      | Opcode.Ping ->
        send @@ Frame.create ~opcode:Opcode.Pong ~content:fr.content ()

      | Opcode.Close ->
        let* _ = Lwt_log.info_f ~section "Client %d sent a close frame" id in
        (* Immediately echo and pass this last message to the user *)
        let* _ = if String.length fr.content >= 2
          then
            send @@ Frame.create ~opcode:Opcode.Close
              ~content:(String.sub fr.content 0 2) ()
          else send @@ Frame.close 1000
        in
        raise @@ ConnectionLost "Close"

      | Opcode.Pong -> Lwt.return_unit

      | Opcode.Text
      | Opcode.Binary -> begin
          let open Sdk.Command in
          let cmd = Yojson.Safe.from_string fr.content in
          let cmd = command_of_yojson cmd in
          match cmd with
          | Ok (FetchData) -> GlobalData.send_data send
          | Ok (Command (id, sub)) -> begin
              let* _ = Lwt_log.info_f ~section "Handle Command ..." in
              cb id (Sdk.Command.subcommand_to_yojson sub)
            end
          | _ -> begin
              let* _ = Lwt_log.info_f ~section "Correct Format but wrong command" in
              raise (Sdk.Command.InvalidCommand fr.content)
            end
        end
      | _ ->
        let* _ = send @@ Frame.close 1002 in
        raise @@ ConnectionLost "Unknown opcode"
    in
    let* x = Connected_client.recv client in
    let* y = react x in
    recv_forever y
  in
  Lwt.catch
    recv_forever (fun exn -> begin
      let* _ = Lwt_log.info_f ~section "Connection to client %d lost" id in
      let* _ = UpdateDispatcher.remove_sender id in
      let* _ = match exn with
      | Sdk.Command.InvalidCommand str -> Lwt_log.info_f ~section "InvalidCommand %s" str
      | _ -> Lwt_log.info_f ~section "%s" (Printexc.to_string exn)
      in
      raise exn
    end)
