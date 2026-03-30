let pick t fa fb = if t then fa 0 else fb 0
let () = println (int_to_str (pick true (fn _ -> 6) (fn _ -> 9)))
