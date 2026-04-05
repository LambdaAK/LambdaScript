Expected:
1
0

Source:
let rec is_even n =
  if n == 0 then 1
  else if n == 1 then 0
  else is_even (n - 2)
let () = print_string (int_to_str (is_even 10))
let () = print_string (int_to_str (is_even 7))
