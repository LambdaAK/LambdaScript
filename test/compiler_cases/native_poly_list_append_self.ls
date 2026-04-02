Expected:
4

Source:
type rec List<'a> = | Nil | Cons of ('a, List<'a>)

let rec append a b =
  case a do
    | Nil -> b
    | Cons (h, t) -> Cons (h, append t b)

let rec len lst =
  case lst do
    | Nil -> 0
    | Cons (_, t) -> 1 + len t

let xs = Cons (1, Cons (2, Nil))
let () = println (int_to_str (len (append xs xs)))
