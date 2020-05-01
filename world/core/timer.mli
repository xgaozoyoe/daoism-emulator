type time_slice

val play: time_slice -> time_slice
val trigger: time_slice -> bool
val of_int: int -> time_slice
val to_string: time_slice -> string
val to_json: time_slice -> Yojson.Basic.t
