Expected:
1
2
3

Source:
let counter_val () = 1
let () = print_string (int_to_str (counter_val ()))
let () = print_string (int_to_str (counter_val () + counter_val ()))
let () = print_string (int_to_str (counter_val () + counter_val () + counter_val ()))
