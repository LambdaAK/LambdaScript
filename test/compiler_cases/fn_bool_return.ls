Expected:
1
0

Source:
let is_positive x = x > 0
let () = println (int_to_str (if is_positive 3 then 1 else 0))
let () = println (int_to_str (if is_positive (0 - 5) then 1 else 0))
