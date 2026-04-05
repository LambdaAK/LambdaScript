Expected:
42

Source:
let always42 _ = 42
let () = print_string (int_to_str (always42 100))
