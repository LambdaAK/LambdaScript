
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
    | Cons (h, t) -> Cons (h, t >>= f)

  let return x = Cons (x, Nil)
}

let o1 = Some 1
let o2 = Some 2
let o3 = Some 3
let o4 = Some 4
let o5 = Some 5
let o6 = Some 6
let o7 = Some 7
let o8 = Some 8
let o9 = Some 9
let o10 = Some 10

let m1 = o5 >>= (fn x -> Some (x + 1))

let () = case m1 do
  | Some v -> println (int_to_str v)
  | None -> println "none"