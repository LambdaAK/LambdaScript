mod ListModule where

  type rec LinkedList<a> =
    | Nil
    | Cons of (a, LinkedList<a>)

  let empty = Nil

  let cons x xs = Cons (x, xs)

  let hd xs =
    case xs do
    | Nil -> None
    | Cons (x, _) -> Some x

  let rec append xs ys =
    case xs do
    | Nil -> ys
    | Cons (x, xs) -> Cons (x, append xs ys)

  let rec reverse xs =
    case xs do
    | Nil -> Nil
    | Cons (x, xs) -> append (reverse xs) (cons x Nil)

end

let my_list = ListModule.cons 1 (ListModule.cons 2 (ListModule.cons 3 ListModule.empty))

let reversed_list = ListModule.reverse my_list