Expected:
3
7

Source:
let min a b = if a < b then a else b
let max a b = if a > b then a else b
let () = println (int_to_str (min 3 7))
let () = println (int_to_str (max 3 7))
