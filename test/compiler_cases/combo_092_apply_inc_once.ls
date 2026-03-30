Expected:
41

Source:
let a f x = f x
let inc v = v + 1
let () = println (int_to_str (a inc 40))
