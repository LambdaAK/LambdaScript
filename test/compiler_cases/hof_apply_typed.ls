Expected:
8

Source:
let call_with_5 (f : Int -> Int) : Int = f 5
let add3 x = x + 3
let () = print_string (int_to_str (call_with_5 add3))
