Expected:
120

Source:
let rec tri n = if n == 0 then 0 else n + tri (n - 1)
let () = println (int_to_str (tri 15))
