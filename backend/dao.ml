open Lwt.Infix
open Lwt.Syntax
open Websocket_lwt_unix
open WebsocketApi

let world = Universe.init Universe.default_config;;

let rec step _ : unit Lwt.t = begin
  let* _ = Lwt_unix.sleep 1.0 in
  let* _ = Lwt.return @@ world#step world#space in
  let* update_info = Lwt.return @@ world#get_update in
  let* _ = UpdateDispatcher.broadcast update_info in
  let* _ = GlobalData.update_data world#to_json in
  step ()
end in

let main uri =
  let handler = WebsocketApi.handler (fun json -> Lwt.return @@ world#handle_command json) in
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
