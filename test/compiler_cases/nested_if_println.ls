Expected:
ok
0

Source:
let () = if false then print_string "skip" else (
  if true then print_string "ok" else print_string "bad"
)
let () = print_string (int_to_str 0)
