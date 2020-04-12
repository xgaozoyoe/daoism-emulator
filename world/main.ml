let world = Universe.init Universe.default_config;;

let events = Universe.step world;;

List.iter (fun e -> Printf.printf "[event:] %s\n" (Universe.Event.to_string e)) events;;
