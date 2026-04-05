Expected:
256

Source:
let rec pow2 n = if n == 0 then 1 else 2 * pow2 (n - 1)
let () = print_string (int_to_str (pow2 8))
