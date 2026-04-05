Expected:
6

Source:
let rec gcd a b = if b == 0 then a else gcd b (a % b)
let () = print_string (int_to_str (gcd 84 30))
