Expected:
13

Source:
let triple (f : Int -> Int) (x : Int) : Int = f (f (f x))
let add1 x = x + 1
let () = println (int_to_str (triple add1 10))
