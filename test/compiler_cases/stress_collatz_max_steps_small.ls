Expected:
3
8

Source:
let rec collatz_len n acc =
  if n == 1 then acc
  else if n % 2 == 0 then collatz_len (n / 2) (acc + 1)
  else collatz_len (3 * n + 1) (acc + 1)

let () = println (int_to_str (collatz_len 8 0))
let () = println (int_to_str (collatz_len 6 0))
