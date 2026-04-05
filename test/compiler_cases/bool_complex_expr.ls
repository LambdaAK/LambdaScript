Expected:
1

Source:
let () = print_string (int_to_str (if (true || false) && (false || true) then 1 else 0))
