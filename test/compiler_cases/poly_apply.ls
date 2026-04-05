Expected:
25

Source:
let apply f x = f x
let square n = n * n
let () = print_string (int_to_str (apply square 5))
