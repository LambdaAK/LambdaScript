Expected:
7

Source:
let run g = g 3 + g 4
let id x = x
let () = println (int_to_str (run id))
