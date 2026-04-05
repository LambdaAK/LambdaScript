Expected:
8

Source:
let id x = x
let h = id
let () = print_string (int_to_str (h 8))
