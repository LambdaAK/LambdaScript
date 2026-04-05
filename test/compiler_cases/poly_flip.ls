Expected:
7

Source:
let flip f x y = f y x
let sub a b = a - b
let () = print_string (int_to_str (flip sub 3 10))
