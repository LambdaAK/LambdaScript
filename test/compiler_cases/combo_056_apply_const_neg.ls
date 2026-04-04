Expected:
-7

Source:
let apply f x = f x
let z (_ : Int) : Int = 0 - 7
let () = println (int_to_str (apply z 100))
