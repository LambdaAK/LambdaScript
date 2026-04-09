type Maybe<a> =
  | Nothing
  | Just of a

impl Show for Maybe<a> requires Show<a> where
  let show o =
    case o do
    | Nothing -> "Nothing"
    | Just v -> "Just(" ^ (show v) ^ ")"
end

type rec LinkedList<a> =
  | Nil
  | Cons of (a, LinkedList<a>)


impl Show for LinkedList<a> requires Show<a> where
  let show l =
    case l do
    | Nil -> "Nil"
    | Cons (h, t) -> "Cons " ^ (show h) ^ " " ^ (show t)
end

let my_list = Cons (1, Nil)

let show_result = show my_list

let () = println show_result