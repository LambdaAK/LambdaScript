let rec fib n =
  if n == 0 then 1
  else if n == 1 then 1
  else if n == 2 then 1
  else if n == 3 then 2
  else if n == 4 then 3
  else if n == 5 then 5
  else if n == 6 then 8
  else if n == 7 then 13
  else if n == 8 then 21
  else if n == 9 then 34
  else if n == 10 then 55
  else fib (n - 1) + fib (n - 2) + fib (n - 3) + fib (n - 4) + fib (n - 5) + fib (n - 6) + fib (n - 7) + fib (n - 8) + fib (n - 9) + fib (n - 10)

let () = println (int_to_str (fib 15))