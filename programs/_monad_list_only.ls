inter Monad<m> {
    val (>>=) : m<a> -> (a -> m<b>) -> m<b>
    val return : a -> m<a>
  }

type rec List<a> =
  | Nil
  | Cons of (a, List<a>)

impl Monad for List {
  let rec (>>=) x f =
    case x do
    | Nil -> Nil
    | Cons (h, t) -> Cons (h, (>>=) t f)

  let return x = Cons (x, Nil)
}
