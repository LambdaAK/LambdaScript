Expected:
6

Source:
let pick t (fa : int -> int) (fb : int -> int) = if t then fa 0 else fb 0
let const6 (x : int) : int = 6
let const9 (x : int) : int = 9
let () = println (int_to_str (pick true const6 const9))
