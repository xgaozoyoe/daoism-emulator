type time_slice = int

type timer_excp = StaleTimeSlice

let of_int i = i

exception TimerException of timer_excp

let play slice =
  if slice == 0 then raise (TimerException StaleTimeSlice) else
  slice - 1

let trigger slice = (slice = 0)

let to_string t = string_of_int t
