Expected:
15

Source:
let rec ma n acc = if n == 0 then acc else ma (n - 1) (acc + n)
let () = print_string (int_to_str (ma 5 0))
