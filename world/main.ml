open Core
open Lwt.Infix
open Lwt.Syntax
open Websocket
open Websocket_lwt_unix

let world = Universe.init Universe.default_config;;

exception ConnectionLost of string

type resp_data = {
  mutable resp: Yojson.Basic.t;
  mutable events: Yojson.Basic.t list;
};;

let resp_data = {
    resp = `String "Welcome";
    events = [];
} in

let resp_lock = Lwt_mutex.create () in

let rec step _ : unit Lwt.t = begin
  let* _ = Lwt_unix.sleep 1.0 in
  let* _ = Lwt.return @@ world#step
    (world:>Object.t) world#space in
  Lwt_mutex.with_lock resp_lock @@ (fun _ ->
    resp_data.resp <- world#to_json;
    Lwt.return ();
  ) >>= fun _ ->
  step ()
end in

let section = Lwt_log.Section.make "daoism" in

let handler id client =
  incr id;
  let id = !id in
  let send = Connected_client.send client in
  Lwt_log.ign_info_f ~section "New connection (id = %d)" id;
  (*
  Lwt.async (fun () ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      send @@ Frame.create ~content:"Welcome" ()
    );
  *)
  let rec recv_forever () =
    let open Frame in
    let react fr =
      Lwt_log.debug_f ~section "<- %s" (Frame.show fr) >>= fun () ->
      match fr.opcode with
      | Opcode.Ping ->
        send @@ Frame.create ~opcode:Opcode.Pong ~content:fr.content ()

      | Opcode.Close ->
        Lwt_log.info_f ~section "Client %d sent a close frame" id >>= fun () ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.content >= 2 then
           send @@ Frame.create ~opcode:Opcode.Close
             ~content:(String.sub fr.content 0 2) ()
         else send @@ Frame.close 1000) >>= fun () ->
        raise @@ ConnectionLost "Close"

      | Opcode.Pong -> Lwt.return_unit

      | Opcode.Text
      | Opcode.Binary ->
        let* buf = Lwt_mutex.with_lock resp_lock @@ (fun _ ->
          Lwt.return @@ Yojson.Basic.to_string resp_data.resp
        ) in
        send @@ Frame.create ~content:buf ()
      | _ ->
        send @@ Frame.close 1002 >>= fun () ->
        raise @@ ConnectionLost "Unknown opcode"
    in
    Connected_client.recv client >>= react >>= recv_forever
  in
  Lwt.catch
    recv_forever
    (fun exn ->
       Lwt_log.info_f ~section "Connection to client %d lost" id >>= fun () ->
       (* Lwt.fail *) raise exn)
in

let main uri =
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let open Conduit_lwt_unix in
  endp_to_server ~ctx:default_ctx endp >>= fun server ->
  let rec wait_for_connect () = Lwt.catch
    (fun _ -> establish_server ~ctx:default_ctx ~check_request:(fun _ -> true) ~mode:server (handler @@ ref (-1)))
    (fun _ -> wait_for_connect ()) in
  wait_for_connect ()

in

Lwt.async_exception_hook := (function
  | Unix.Unix_error (error, func, arg) ->
    Logs.warn (fun m ->
      m  "Client connection error %s: %s(%S)"
        (Unix.error_message error) func arg
    )
  | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
);

let () =
  let uri = ref "http://0.0.0.0:9001" in

  let speclist = Arg.align
      [
        "-v", Arg.String (fun s -> Lwt_log.(add_rule s Info)), "<section> Put <section> to Info level";
        "-vv", Arg.String (fun s -> Lwt_log.(add_rule s Debug)), "<section> Put <section> to Debug level"
      ]
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;

  ignore @@ main @@ Uri.of_string !uri;
  Lwt_main.run @@ step ()

in

();
