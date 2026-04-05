Expected:
43

Source:
let add3 x y z = x + y + z
let p = add3 1
let q = p 2
let () = print_string (int_to_str (q 40))
