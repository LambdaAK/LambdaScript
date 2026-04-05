Expected:
42

Source:
let c x _ = x
let () = print_string (int_to_str (c 42 0))
