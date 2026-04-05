Expected:
1
2
3

Source:
let f x = x
let () = print_string (int_to_str (f 1))
let () = print_string (int_to_str (f 2))
let () = print_string (int_to_str (f 3))
