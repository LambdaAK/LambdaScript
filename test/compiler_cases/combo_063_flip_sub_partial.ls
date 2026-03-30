Expected:
7

Source:
let flip f x y = f y x
let sub a b = a - b
let g = flip sub 3
let () = println (int_to_str (g 10))
