Expected:
-7

Source:
let apply f x = f x
let z (_ : Int) : Int = 0 - 7
let () = print_string (int_to_str (apply z 100))
