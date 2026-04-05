Expected:
15

Source:
let rec nth_odd n =
  if n == 1 then 1 else 2 + nth_odd (n - 1)

let () = print_string (int_to_str (nth_odd 8))
