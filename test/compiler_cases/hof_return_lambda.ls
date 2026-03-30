Expected:
42

Source:
let get_double () : int -> int = fn x -> x * 2
let () = println (int_to_str ((get_double ()) 21))
