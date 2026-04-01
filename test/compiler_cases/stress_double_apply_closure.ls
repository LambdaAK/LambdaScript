Expected:
27

Source:
let wrap = fn x -> fn y -> x * y + x

let () = println (int_to_str ((wrap 3) 8))
