Expected:
5050

Source:
let rec sum n = if n == 0 then 0 else n + sum (n - 1)
let () = print_string (int_to_str (sum 100))
