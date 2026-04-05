Expected:
120

Source:
let rec tri n = if n == 0 then 0 else n + tri (n - 1)
let () = print_string (int_to_str (tri 15))
