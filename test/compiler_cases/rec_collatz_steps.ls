Expected:
8

Source:
let rec collatz n =
  if n == 1 then 0
  else if n % 2 == 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1)
let () = println (int_to_str (collatz 6))
