Expected:
15

Source:
let k x f = f x
let inc z = z + 10
let () = println (int_to_str (k 5 inc))
