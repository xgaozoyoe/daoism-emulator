type connection
external ws: connection = "ws" [@@bs.val]
external send_command: connection -> string -> unit = "send" [@@bs.send]
