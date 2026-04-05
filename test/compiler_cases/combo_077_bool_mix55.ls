Expected:
55

Source:
let () = print_string (int_to_str (if (1 < 2) && (3 < 4) || false then 55 else 0))
