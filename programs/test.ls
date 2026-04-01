let id x y = x

let a = (id (id id true) id) 1 (fn x -> x)

let () = println (int_to_str a)