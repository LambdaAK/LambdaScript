Expected:
55

Source:
let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
let () = print_string (int_to_str (fib 10))
