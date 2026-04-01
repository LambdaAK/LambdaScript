Expected:
100
42

Source:
let abs x = if x < 0 then 0 - x else x

let () = println (int_to_str (abs (0 - 100)))
let () = println (int_to_str (abs 42))
