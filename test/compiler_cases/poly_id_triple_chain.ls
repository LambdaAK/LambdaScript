Expected:
5

Source:
let id x = x
let () = println (int_to_str ((id id id) 5))
