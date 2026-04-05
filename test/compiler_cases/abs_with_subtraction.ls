Expected:
12

Source:
let abs x = if x < 0 then 0 - x else x
let () = print_string (int_to_str (abs (0 - 12)))
