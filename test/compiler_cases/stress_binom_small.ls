Expected:
210

Source:
let rec fact n = if n <= 1 then 1 else n * fact (n - 1)

let binom n k = fact n / (fact k * fact (n - k))

let () = println (int_to_str (binom 10 4))
