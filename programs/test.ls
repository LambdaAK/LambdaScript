inter Monoid<a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [a] {
  let rec mappend x y =
    case x do
      | [] -> y
      | h :: t -> h :: mappend t y

  let mempty = []
}

inter Monad<m> {
    val (>>=) : m<a> -> (a -> m<b>) -> m<b>
    val return : a -> m<a>
  }

type Option<a> =
  | None
  | Some of a

impl Monad for Option {
  let (>>=) x f =
    case x do
    | None -> None
    | Some v -> f v
  let return x = Some x
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

