let rec even n =
  if n == 0 then true
  else odd (n - 1)

and odd n =
  if n == 0 then false
  else even (n - 1)

let () = println (int_to_str (if odd 7 then 1 else 0))
