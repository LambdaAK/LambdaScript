Expected:
42
1

Source:
let const x _ = x
let () = print_string (int_to_str (const 42 99))
let () = print_string (int_to_str (if const true false then 1 else 0))
