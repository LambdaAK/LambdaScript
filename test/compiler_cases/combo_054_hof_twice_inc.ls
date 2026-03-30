Expected:
10

Source:
let twice f x = f (f x)
let inc n = n + 1
let () = println (int_to_str (twice inc 8))
