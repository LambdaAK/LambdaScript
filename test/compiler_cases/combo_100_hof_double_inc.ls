Expected:
14

Source:
let s f g x = f (g x)
let double x = x * 2
let inc y = y + 1
let () = println (int_to_str (s double inc 6))
