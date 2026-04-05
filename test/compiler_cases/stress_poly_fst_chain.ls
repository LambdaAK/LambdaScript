Expected:
100

Source:
let fst a b = a

let () = print_string (int_to_str (fst (fst (fst 100 1) 2) 3))
