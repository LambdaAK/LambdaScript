Expected:
12

Source:
let f n = if n < 0 then 0 else n
let () = print_string (int_to_str (f (0 - 5) + f 12))
