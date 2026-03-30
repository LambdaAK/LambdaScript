Expected:
1
2
3

Source:
let counter_val () = 1
let () = println (int_to_str (counter_val ()))
let () = println (int_to_str (counter_val () + counter_val ()))
let () = println (int_to_str (counter_val () + counter_val () + counter_val ()))
