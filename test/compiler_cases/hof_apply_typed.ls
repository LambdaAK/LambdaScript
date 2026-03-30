Expected:
8

Source:
let call_with_5 (f : int -> int) : int = f 5
let add3 x = x + 3
let () = println (int_to_str (call_with_5 add3))
