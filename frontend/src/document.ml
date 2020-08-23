type dom
type element
external document : dom = "document" [@@bs.val]
external get_by_id: dom -> string -> element = "getElementById" [@@bs.send]
external add_event_listener: element -> string -> 'a -> unit = "addEventListener" [@@bs.send]
external innerHTML : element -> string = "" [@@bs.get]
external setInnerHTML : element -> string -> unit = "innerHTML" [@@bs.set]
external setOuterHTML : element -> string -> unit = "outerHTML" [@@bs.set]

external appendChild: element -> element -> unit = "appendChild" [@@bs.send]
external createElement : dom -> string -> element
    = "createElement" [@@bs.send]
