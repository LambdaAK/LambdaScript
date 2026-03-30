Expected:
7

Source:
let min a b = if a < b then a else b
let max a b = if a > b then a else b
let () = println (int_to_str (min (max 3 9) 7))
