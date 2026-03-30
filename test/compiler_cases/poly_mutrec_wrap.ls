Expected:
3

Source:
let rec wrap x = x
and unwrap y = wrap y
let () = println (int_to_str (unwrap 3))
