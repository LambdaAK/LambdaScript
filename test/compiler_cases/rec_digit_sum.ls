Expected:
15

Source:
let rec digit_sum n =
  if n == 0 then 0
  else n % 10 + digit_sum (n / 10)
let () = print_string (int_to_str (digit_sum 12345))
