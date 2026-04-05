Expected:
18

Source:
let compose f g x = f (g x)
let add2 x = x + 2
let mul3 x = x * 3
let () = print_string (int_to_str (compose mul3 add2 4))
