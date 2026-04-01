Expected:
1

Source:
let id x y = x

let a = (id (id id 10) true) 1 (fn x -> x)

let () = println (int_to_str a)
