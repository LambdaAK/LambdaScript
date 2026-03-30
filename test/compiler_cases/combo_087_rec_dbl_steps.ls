Expected:
16

Source:
let rec d n k = if k == 0 then n else d (n * 2) (k - 1)
let () = println (int_to_str (d 1 4))
