Expected:
8

Source:
let double x = x * 2
let () = print_string (int_to_str (2 |> double |> double))
