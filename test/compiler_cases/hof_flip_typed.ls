Expected:
7

Source:
let flip (f : Int -> Int -> Int) (x : Int) (y : Int) : Int = f y x
let sub a b = a - b
let () = println (int_to_str (flip sub 3 10))
