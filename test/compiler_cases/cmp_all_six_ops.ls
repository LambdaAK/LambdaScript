Expected:
1
1
1
1
1
1

Source:
let () = print_string (int_to_str (if 3 == 3 then 1 else 0))
let () = print_string (int_to_str (if 3 == 4 then 0 else 1))
let () = print_string (int_to_str (if 3 < 4 then 1 else 0))
let () = print_string (int_to_str (if 3 <= 3 then 1 else 0))
let () = print_string (int_to_str (if 4 > 3 then 1 else 0))
let () = print_string (int_to_str (if 4 >= 4 then 1 else 0))
