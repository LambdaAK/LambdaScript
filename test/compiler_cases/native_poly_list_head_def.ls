Expected:
5

Source:
type rec List<'a> = | Nil | Cons of ('a, List<'a>)

let rec head_def d lst =
  case lst do
    | Nil -> d
    | Cons (h, _) -> h

let () = println (int_to_str (head_def 0 (Cons (5, Nil))))
