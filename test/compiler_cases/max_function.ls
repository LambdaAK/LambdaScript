Expected:
10

Source:
let max a b = if a > b then a else b
let () = print_string (int_to_str (max 3 10))
