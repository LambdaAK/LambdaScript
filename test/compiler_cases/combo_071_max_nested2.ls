Expected:
5

Source:
let m a b = if a > b then a else b
let () = println (int_to_str (m (m 2 5) 3))
