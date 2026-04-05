Expected:
0

Source:
let g x = x
let f y = g y
let () = print_string (int_to_str (f 0))
