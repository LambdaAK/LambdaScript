Expected:
43

Source:
let c f g x = f (g x)
let id (x : Int) : Int = x
let () = println (int_to_str (c id id 43))
