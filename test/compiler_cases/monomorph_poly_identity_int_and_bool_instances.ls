Expected:
0
1

Source:
let f x = x
let () = print_string (int_to_str (f 0))
let () = print_string (int_to_str (if f true then 1 else 0))
