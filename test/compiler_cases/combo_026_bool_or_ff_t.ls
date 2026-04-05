Expected:
7

Source:
let () = print_string (int_to_str (if false || true then 7 else 0))
