Expected:
7
90
0

Source:
let flip f x y = f y x

let sub a b = a - b

let rec odd n =
  if n == 0 then false
  else if n == 1 then true
  else odd (n - 2)

let () = print_string (int_to_str (flip sub 3 10))
let () = print_string (int_to_str (flip sub 10 100))
let () = print_string (int_to_str (if odd 7 then 0 else 1))
