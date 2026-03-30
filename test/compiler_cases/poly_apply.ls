Expected:
25

Source:
let apply f x = f x
let square n = n * n
let () = println (int_to_str (apply square 5))
