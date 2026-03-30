Expected:
7

Source:
let () = println (int_to_str ((fn x -> fn y -> x + y) 3 4))
