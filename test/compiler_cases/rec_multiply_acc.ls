Expected:
24

Source:
let rec product n =
  let rec go n acc =
    if n == 0 then acc
    else go (n - 1) (n * acc)
  in
  go n 1
let () = println (int_to_str (product 4))
