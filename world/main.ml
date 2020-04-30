open Core
open Lwt.Infix
let world: Object.t = Universe.init Universe.default_config;;

let rec step _ : unit Lwt.t =
  Lwt_unix.sleep 1.0 >>= fun _ ->
  Lwt.return @@ world#step world >>= fun _ ->
  step ()
in

Lwt_main.run @@ step ();;


