type rec L = | Nil | C of (int, L)

let x = C (1, Nil)

let () = println (int_to_str (case x do | Nil -> 0 | C _ -> 1))
