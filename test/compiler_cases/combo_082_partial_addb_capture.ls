Expected:
42

Source:
let addb n x = x + n
let g = addb 10
let () = print_string (int_to_str (g 32))
