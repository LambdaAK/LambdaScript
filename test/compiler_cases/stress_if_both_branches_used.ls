Expected:
30
60

Source:
let pick t a b = if t then a else b

let () = println (int_to_str (pick true 30 999))
let () = println (int_to_str (pick false 100 60))
