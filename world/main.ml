let world = Universe.init Universe.default_config in

let events = Universe.one_step world in

List.iter (fun e -> Printf.printf "[event:] %s\n" (Universe.Event.to_string e))events;;
