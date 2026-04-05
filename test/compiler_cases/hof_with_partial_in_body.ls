Expected:
8

Source:
let add x y = x + y
let map_add5 (f : Int -> Int -> Int) (x : Int) : Int = f 5 x
let () = print_string (int_to_str (map_add5 add 3))
