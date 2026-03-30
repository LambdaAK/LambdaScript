Expected:
100

Source:
let double x = x * 2
let add10 x = x + 10
let () = println (int_to_str (double (add10 (double (double 10)))))
