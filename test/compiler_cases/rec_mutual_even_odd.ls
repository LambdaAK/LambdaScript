Expected:
1
0
0
1

Source:
let rec even n = if n == 0 then 1 else odd (n - 1)
and odd n = if n == 0 then 0 else even (n - 1)
let () = print_string (int_to_str (even 4))
let () = print_string (int_to_str (even 3))
let () = print_string (int_to_str (odd 4))
let () = print_string (int_to_str (odd 3))
