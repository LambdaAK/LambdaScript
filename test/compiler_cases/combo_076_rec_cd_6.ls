Expected:
21

Source:
let rec cd n = if n == 1 then 1 else n + cd (n - 1)
let () = print_string (int_to_str (cd 6))
