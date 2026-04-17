Expected:
55

Source:
let triangle n =
  let rec go n acc =
    if n == 0 then acc
    else go (n - 1) (acc + n)
  in
  go n 0
let () = print_string (int_to_str (triangle 10))
