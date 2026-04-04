Expected:
3
0

Source:
type rec List<a> = | Nil | Cons of (a, List<a>)

let rec length lst =
  case lst do
    | Nil -> 0
    | Cons (_, t) -> 1 + length t

let () = println (int_to_str (length (Cons (1, Cons (2, Cons (3, Nil))))))
let () = println (int_to_str (length Nil))
