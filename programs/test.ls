type Option<'a> = | None | Some of 'a

type List<'a> = | Nil | Cons of ('a, List<'a>)

let rec length lst =
  case lst do
    | Nil -> 0
    | Cons (_, t) -> 1 + length t

let head lst =
  case lst do
    | Nil -> None
    | Cons (h, _) -> Some h

let rec append lst1 lst2 =
  case lst1 do
    | Nil -> lst2
    | Cons (h, t) -> Cons (h, append t lst2)

let rec rev lst =
  case lst do
    | Nil -> Nil
    | Cons (h, t) -> append (rev t) (Cons (h, Nil))


let lst1 = Nil
let lst2 = Cons (1, Nil)
let lst3 = Cons (1, Cons (2, Cons (3, Nil)))

let () = println (int_to_str (length lst1))
let () = println (int_to_str (length lst3))

let print_option_int o =
  case o do
    | None -> println "None"
    | Some x -> println (int_to_str x)

let () = print_option_int (head lst1)
let () = print_option_int (head lst2)

