Expected:
11

Source:
let c f g x = f (g x)
let a x = x + 1
let b x = x * 2
let () = println (int_to_str (c a b 5))
