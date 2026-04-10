Expected:
3

Source:
mod ListModule where
  type rec LinkedList<a> =
    | Nil
    | Cons of (a, LinkedList<a>)

  trait Appendable<a> where
    val append_one : a -> a
  end

  impl Appendable for LinkedList<a> where
    append_one xs = xs
  end

  let empty = Nil
  let cons x xs = Cons (x, xs)

  let rec len xs =
    case xs do
    | Nil -> 0
    | Cons (_, t) -> 1 + len t

  let my_list = cons 1 (cons 2 (cons 3 empty))
  let out = len (append_one my_list)
end

let () = print_string (int_to_str ListModule.out)
