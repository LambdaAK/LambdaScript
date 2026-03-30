Expected:
42
1

Source:
let const x _ = x
let () = println (int_to_str (const 42 99))
let () = println (int_to_str (if const true false then 1 else 0))
