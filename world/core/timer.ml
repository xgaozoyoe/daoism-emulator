type time_slice = int

type timer_excp = StaleTimeSlice

exception TimerException of timer_excp

let play slice =
  if slice == 0 then raise (TimerException StaleTimeSlice) else
  slice - 1

let trigger slice = (slice = 1)
