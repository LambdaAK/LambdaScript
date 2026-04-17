Expected:
0

Source:
mod ListModule where
  type rec LinkedList<a> =
    | Nil
    | Cons of (a, LinkedList<a>)

  let rec len xs =
    case xs do
    | Nil -> 0
    | Cons (_, t) -> 1 + len t

  let empty = Nil
end

use ListModule
let () = print_string (int_to_str (len empty))
