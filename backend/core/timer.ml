open Lwt.Infix

type slice = int

type timer_excp = StaleTimeSlice

let of_int i = i

exception TimerException of timer_excp

let play slice =
  if slice == 0 then raise (TimerException StaleTimeSlice) else
  slice - 1

let trigger slice = (slice = 0)

let to_string t = string_of_int t
let to_json t = `Int t

module TriggerQueue = struct
  type 'a t =
    | Tail
    | Head of {
        mutable next: 'a t;
      }
    | Entity of {
        value: 'a;
        mutable wait: slice;
        mutable prev: ('a t) ref;
        mutable next: 'a t;
     }

  let empty = Tail
  let mk_head x = Head {next = x}

  let mk_event (e:'a) l prev next :'a t =
    let local = ref Tail in
    let next = match next with
      | Tail -> Tail
      | Entity e -> Entity {e with prev = local}
      | Head _ -> assert false
    in
    local := Entity {
       value = e; wait = l; next = next; prev = prev;
    };
    !local

  let set_next a n = match a with
    | Tail -> assert false
    | Entity e -> e.next <- n
    | Head h -> h.next <- n

  let register_event w e head = begin
    let rec register w prev current = match !current with
    | Tail -> begin
        let e = mk_event e w prev Tail in
        set_next (!prev) @@ e; e
      end
    | Head _ -> assert false
    | Entity evt -> begin
      begin
        let t = evt.wait in
        if t > w
        then begin (* event happens before hd *)
          let e = mk_event e w (evt.prev) (Entity {evt with wait = t-w}) in
          set_next (!prev) @@ e; e
        end else (* event happens after hd *) begin
          let e = register (w-t) current (ref (evt.next)) in
          e
        end
      end
    end in
    let current = match !head with
    | Head d -> ref (d.next)
    | _ -> assert false
    in
    register w head current;
  end

  let cancel_event e = match e with
  | Entity evt -> begin
      let b = evt.next in
      let tail = match b with
      | Head _ -> assert false
      | Tail -> Tail
      | Entity e -> e.prev <- evt.prev; Entity e
      in
      match !(evt.prev) with
      | Tail -> ()
      | Head d -> d.next <- tail
      | Entity e -> begin
          e.next <- tail
        end
    end
  | _ -> assert false

  let fetch_events head dec = begin
    let rec aux es h = match h with
      | Entity e when e.wait = 0 ->
          let next = e.next in
          e.prev <- ref Tail;
          e.next <- Tail;
          aux (e.value :: es) next
      | Entity e -> begin
          e.prev <- head;
          e.wait <- if dec then e.wait - 1 else e.wait;
          es, Entity e
        end
      | Tail -> es, Tail
      | Head d -> begin
          let es, tail = aux es d.next in
          d.next <- tail;
          es, h
        end
    in
    let es, h = aux [] !head in
    head := h;
    es
  end

  let print_node head to_str = match head with
    | Entity e -> to_str e.value
    | Tail -> "tail"
    | Head _ -> "head"

  let rec dump head to_str = match head with
    | Entity e ->
        Lwt_io.printf "%s:%d (%s) -> " (to_str e.value) e.wait (print_node !(e.prev) to_str) >>=
        fun _ -> dump e.next to_str
    | Tail -> Lwt_io.printf "nil\n"
    | Head d ->
        Lwt_io.printf "Head -> " >>=
        fun _ -> dump d.next to_str
end
