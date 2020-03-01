let world = Universie.init Universe.default_config in

let events = world.one_step ()

List.iter (fun e -> Log.log "[event:] %s\n" Event.to_string event)
