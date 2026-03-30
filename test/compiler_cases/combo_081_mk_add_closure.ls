Expected:
123

Source:
let mk_add n = fn x -> x + n
let g = mk_add 100
let () = println (int_to_str (g 23))
