Expected:
5
0
10

Source:
let clamp lo hi x =
  if x < lo then lo
  else if x > hi then hi
  else x
let () = print_string (int_to_str (clamp 0 10 5))
let () = print_string (int_to_str (clamp 0 10 (0 - 3)))
let () = print_string (int_to_str (clamp 0 10 15))
