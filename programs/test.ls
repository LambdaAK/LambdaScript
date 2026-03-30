let add_three (x: int) (y: int) (z: int) : int = x + y + z

let add (x: int) (y: int) : int = x + y

let apply (f : int -> int -> int) (x : int) (y: int) : int = f x y

let partially_applied = add_three 1

let done = apply partially_applied 2 3

let () = println (int_to_str done)
