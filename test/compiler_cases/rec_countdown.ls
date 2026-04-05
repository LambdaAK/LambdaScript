Expected:
0

Source:
let rec countdown n = if n == 0 then 0 else countdown (n - 1)
let () = print_string (int_to_str (countdown 50))
