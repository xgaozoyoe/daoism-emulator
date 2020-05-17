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
    | Entity of {
        value: 'a;
        wait: slice;
        mutable next: 'a t;
     }

  let mk_event e l next = Entity {value = e; wait = l; next = next}

  let register_event w e head =
    let rec register w tail =
    match tail with
    | Tail -> mk_event e w Tail
    | Entity evt ->
      begin
        let t = evt.wait in
        if t > w
        then (* event happens before hd *)
          mk_event e w (Entity {evt with wait = t-w})
        else (* event happens after hd *) begin
          evt.next <- register (w-t) (evt.next);
          Entity evt
        end
      end
    in
    register w head

  let rec fetch_events es head = match head with
    | Entity e when e.wait = 0 ->
        fetch_events (e.value :: es) e.next
    | Entity e -> es, Entity {value = e.value; wait = e.wait -1; next = e.next}
    | Tail -> es, Tail

end
