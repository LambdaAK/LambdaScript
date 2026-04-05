Expected:
25

Source:
let square x = x * x
let () = print_string (int_to_str (square 3 + square 4))
