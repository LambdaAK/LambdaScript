Expected:
7
42
1

Source:
let id x y = x

let k x y = y

let () = print_string (int_to_str (id (k 100 7) 0))
let () = print_string (int_to_str (id 42 (id 99 100)))
let () = print_string (int_to_str ((id (id id 10) true) 1 (fn x -> x)))
