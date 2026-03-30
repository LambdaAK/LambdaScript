Expected:
20

Source:
let rec c n k = if k == 0 || k == n then 1 else c (n - 1) (k - 1) + c (n - 1) k
let () = println (int_to_str (c 6 3))
