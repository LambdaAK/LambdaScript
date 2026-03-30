Expected:
720

Source:
let rec fact n = if n == 0 then 1 else n * fact (n - 1)
let () = println (int_to_str (fact 6))
