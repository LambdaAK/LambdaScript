Expected:
1

Source:
let flip f x y = f y x
let sub a b = a - b
let () = println (int_to_str (flip sub 8 9))
