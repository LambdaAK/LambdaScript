Expected:
18

Source:
let both (f : int -> int) (x : int) : int = f x + f x
let square x = x * x
let () = println (int_to_str (both square 3))
