Expected:
7
42
1

Source:
let id x y = x

let k x y = y

let () = println (int_to_str (id (k 100 7) 0))
let () = println (int_to_str (id 42 (id 99 100)))
let () = println (int_to_str ((id (id id 10) true) 1 (fn x -> x)))
