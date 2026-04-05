Expected:
11

Source:
let min2 a b = if a < b then a else b

let min3 a b c = min2 (min2 a b) c

let () = print_string (int_to_str (min3 44 11 99))
