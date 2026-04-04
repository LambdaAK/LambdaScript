Expected:
7

Source:
let twice (f : Int -> Int) (x : Int) : Int = f (f x)
let add1 z = z + 1
let () = println (int_to_str (twice add1 5))
