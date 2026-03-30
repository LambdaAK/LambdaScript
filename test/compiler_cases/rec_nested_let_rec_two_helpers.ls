Expected:
1
0

Source:
let check_even n =
  let rec go n = if n == 0 then 1 else if n == 1 then 0 else go (n - 2)
  in
  go n
let () = println (int_to_str (check_even 8))
let () = println (int_to_str (check_even 9))
