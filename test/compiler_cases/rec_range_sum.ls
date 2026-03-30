Expected:
55

Source:
let rec range_sum lo hi =
  if lo > hi then 0
  else lo + range_sum (lo + 1) hi
let () = println (int_to_str (range_sum 1 10))
