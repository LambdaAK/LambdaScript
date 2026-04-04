Expected:
42

Source:
let apply (f : Int -> Int) (x : Int) : Int = f x
let inc y = y + 1
let () = println (int_to_str (apply inc 41))
