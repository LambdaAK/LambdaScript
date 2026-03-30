Expected:
12

Source:
let f x = x
let () = println (int_to_str (f (f (f 12))))
