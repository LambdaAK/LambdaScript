Expected:
72

Source:
let t =
  if 1 == 1 then
    if 2 == 2 then 8 * 9 else 0
  else 1

let () = print_string (int_to_str t)
