Expected:
42

Source:
let get_double () : Int -> Int = fn x -> x * 2
let () = println (int_to_str ((get_double ()) 21))
