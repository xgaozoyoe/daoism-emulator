let world = Universe.init Universe.default_config;;

let _ = Universe.step world in
let _ = Universe.step world in
let _ = Universe.step world in
let _ = Universe.step world in
let _ = Universe.step world in
let events = Universe.step world in

List.iter (fun e -> Printf.printf "[event:] %s\n" (Universe.Event.to_string e)) events;;
