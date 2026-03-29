let apply (f : int -> int) (x : int) : int = f x

let result = apply (fn x -> x + 1) 41

let () = println (int_to_str result)