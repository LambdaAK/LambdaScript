Expected:
55

Source:
let rec fib_iter a b n =
  if n == 0 then a
  else fib_iter b (a + b) (n - 1)
let () = println (int_to_str (fib_iter 0 1 10))
