Expected:
30

Source:
let rec sum_sq n = if n == 0 then 0 else n * n + sum_sq (n - 1)
let () = println (int_to_str (sum_sq 4))
