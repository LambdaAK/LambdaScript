Expected:
17

Source:
let f x y z = x - y + z
let a = f 20
let b = a 7
let () = print_string (int_to_str (b 4))
