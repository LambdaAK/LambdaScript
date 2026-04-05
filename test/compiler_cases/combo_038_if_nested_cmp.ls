Expected:
99

Source:
let () = print_string (int_to_str (if 3 < 4 then if 4 < 5 then 99 else 1 else 2))
