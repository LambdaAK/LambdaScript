Expected:
42

Source:
let apply f x = f x
let twice x = x * 2
let () = print_string (int_to_str (apply twice 21))
