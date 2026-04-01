Expected:
256

Source:
let rec sq n = if n <= 0 then 1 else sq (n - 1) * 2

let () = println (int_to_str (sq 8))
