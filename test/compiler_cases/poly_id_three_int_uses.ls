Expected:
1
2
3

Source:
let f x = x
let () = println (int_to_str (f 1))
let () = println (int_to_str (f 2))
let () = println (int_to_str (f 3))
