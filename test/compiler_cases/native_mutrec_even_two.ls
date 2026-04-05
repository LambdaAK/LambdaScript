Expected:
1

Source:
let result =
  let rec even n = if n == 0 then 1 else odd (n - 1)
  and odd n = if n == 0 then 0 else even (n - 1)
  in even 4
let () = print_string (int_to_str result)
