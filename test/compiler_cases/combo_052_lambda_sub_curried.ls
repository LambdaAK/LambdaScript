Expected:
42

Source:
let () = print_string (int_to_str ((fn x -> fn y -> x - y) 50 8))
