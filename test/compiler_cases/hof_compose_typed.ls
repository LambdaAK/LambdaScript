Expected:
7

Source:
let compose (f : Int -> Int) (g : Int -> Int) (x : Int) : Int = f (g x)
let add1 x = x + 1
let double x = x * 2
let () = println (int_to_str (compose add1 double 3))
