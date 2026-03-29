Expected:
120

Source:
let fact n =
  let rec fact_helper n acc =
    if n == 0 then acc
    else fact_helper (n - 1) (n * acc)
  in
  fact_helper n 1

let () = println (int_to_str (fact 5))
