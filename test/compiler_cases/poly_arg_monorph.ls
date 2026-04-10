Expected:
1

Source:
let apply g = g 1
let id x = x
let () = print_string (int_to_str (apply id))
