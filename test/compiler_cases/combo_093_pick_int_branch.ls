Expected:
6

Source:
let pick t (fa : Int -> Int) (fb : Int -> Int) = if t then fa 0 else fb 0
let const6 (x : Int) : Int = 6
let const9 (x : Int) : Int = 9
let () = print_string (int_to_str (pick true const6 const9))
