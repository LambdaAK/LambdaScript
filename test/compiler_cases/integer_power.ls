Expected:
1024

Source:
let rec pow b e =
  if e == 0 then 1
  else b * pow b (e - 1)

let () = println (int_to_str (pow 2 10))
