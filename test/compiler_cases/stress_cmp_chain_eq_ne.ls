Expected:
1
1
1
0

Source:
let () = println (int_to_str (if 3 == 3 then 1 else 0))
let () = println (int_to_str (if 3 == 4 then 0 else 1))
let () = println (int_to_str (if (1 < 2) && (2 < 3) then 1 else 0))
let () = println (int_to_str (if (5 > 5) then 1 else 0))
