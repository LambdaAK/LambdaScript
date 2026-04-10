mod ListModule where

  type rec LinkedList<a> =
    | Nil
    | Cons of (a, LinkedList<a>)

  impl Functor for LinkedList where
    fmap f xs =
      case xs do
      | Nil -> Nil
      | Cons (x, xs) -> Cons (f x, fmap f xs)
  end

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

use ListModule

let my_list = cons 1 (cons 2 (cons 3 empty))

let reversed_list = reverse my_list