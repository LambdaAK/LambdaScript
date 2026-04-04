
inter Monad<m> {
    val (>>=) : m<a> -> (a -> m<b>) -> m<b>
    val return : a -> m<a>
  }

inter Foldable<f> {
  val fold_left : (b -> a -> b) -> b -> f<a> -> b
  val fold_right : (a -> b -> b) -> b -> f<a> -> b
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

impl Foldable for List {
  let rec fold_left f acc xs =
    case xs do
    | Nil -> acc
    | Cons (h, t) -> fold_left f (f acc h) t
  let rec fold_right f acc xs =
    case xs do
    | Nil -> acc
    | Cons (h, t) -> fold_right f (f h acc) t
}

impl Monad for List {
  let rec (>>=) x f =
    case x do
    | Nil -> Nil
    | Cons (h, t) -> Cons (h, t >>= f)

  let return x = Cons (x, Nil)
}

let my_list = Cons (1, Cons (2, Cons (3, Nil)))

let res = fold_left (fn acc -> fn x -> acc + x) 0 my_list

let () = println (int_to_str res)

let fold<Foldable t> (f : b -> a -> b) (acc : b) (xs : t<a>) = fold_left f acc xs

let f<Monad m> (x : a) = return x