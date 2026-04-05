Expected:
4

Source:
let rec deep n f x = if n == 0 then x else deep (n - 1) f (f x)
let inc t = t + 1
let () = print_string (int_to_str (deep 4 inc 0))
