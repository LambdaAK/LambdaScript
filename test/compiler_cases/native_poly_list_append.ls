Expected:
3

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

let u = Cons (1, Nil)
let v = Cons (2, Cons (3, Nil))
let w = append u v

let () = println (int_to_str (len w))
