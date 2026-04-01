Expected:
100

Source:
let fst a b = a

let () = println (int_to_str (fst (fst (fst 100 1) 2) 3))
