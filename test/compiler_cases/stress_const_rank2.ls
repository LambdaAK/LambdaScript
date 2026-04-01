Expected:
11
7
100

Source:
let const x _ = x

let () = println (int_to_str (const 11 false))
let () = println (int_to_str (const 7 (const 3 9)))
let () = println (int_to_str (const 100 0))
