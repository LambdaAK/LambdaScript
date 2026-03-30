Expected:
10

Source:
let compose f g x = f (g x)
let add1 x = x + 1
let double x = x * 2
let () = println (int_to_str (compose double add1 4))
