Expected:
33

Source:
let f a b c = a + b + c
let p = f 1
let q = p 2
let () = println (int_to_str (q 30))
