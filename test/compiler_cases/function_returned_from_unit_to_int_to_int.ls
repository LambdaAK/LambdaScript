Expected:
41

Source:
let get_inc () : Int -> Int = fn x -> x + 1
let () = println (int_to_str ((get_inc ()) 40))
