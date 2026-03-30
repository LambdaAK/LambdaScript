Expected:
0

Source:
let rec countdown n = if n == 0 then 0 else countdown (n - 1)
let () = println (int_to_str (countdown 50))
