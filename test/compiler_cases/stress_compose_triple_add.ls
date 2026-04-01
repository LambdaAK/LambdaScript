Expected:
33

Source:
let rec bump n x = if n == 0 then x else bump (n - 1) (x + 1)

let () = println (int_to_str (bump 3 30))
