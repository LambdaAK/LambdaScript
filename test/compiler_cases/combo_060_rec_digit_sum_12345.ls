Expected:
15

Source:
let rec ds n = if n == 0 then 0 else n % 10 + ds (n / 10)
let () = print_string (int_to_str (ds 12345))
