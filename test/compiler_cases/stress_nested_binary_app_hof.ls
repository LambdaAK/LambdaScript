Expected:
30

Source:
let add2 a b = a + b

let lift1 f = fn x -> f x x

let t = lift1 add2 15

let () = println (int_to_str t)
