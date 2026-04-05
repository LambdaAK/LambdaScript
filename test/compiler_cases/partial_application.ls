Expected:
7

Source:
let add x y = x + y
let g = add 3
let () = print_string (int_to_str (g 4))
