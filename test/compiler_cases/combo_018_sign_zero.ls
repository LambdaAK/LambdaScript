Expected:
0

Source:
let sign x = if x > 0 then 1 else if x < 0 then 0 - 1 else 0
let () = println (int_to_str (sign 0))
