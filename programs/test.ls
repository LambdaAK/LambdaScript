type Maybe<a> =
  | Nothing
  | Just of a

impl Show for Maybe<a> requires Show<a> where
  show o =
    case o do
    | Nothing -> "Nothing"
    | Just v -> "Just(" ^ (show v) ^ ")"
end

type rec LinkedList<a> =
  | Nil
  | Cons of (a, LinkedList<a>)

type rec Tree<a> =
  | Leaf
  | Node of (a, Tree<a>, Tree<a>)

type rec NTree<a> =
  | NLeaf
  | NNode of (a, List<NTree<a>>)


impl Show for LinkedList<a> requires Show<a> where
  show l =
    case l do
    | Nil -> "Nil"
    | Cons (h, t) -> "Cons (" ^ (show h) ^ ", " ^ (show t) ^ ")"
end

impl Show for Tree<a> requires Show<a> where
  show t =
    case t do
    | Leaf -> "Leaf"
    | Node (v, left, right) -> "Node (" ^ (show v) ^ ", " ^ (show left) ^ ", " ^ (show right) ^ ")"
end

impl Show for NTree<a> requires Show<a> where
  show t =
    case t do
    | NLeaf -> "NLeaf"
    | NNode (v, children) -> "NNode (" ^ (show v) ^ ", " ^ (show children) ^ ")"
end

let my_list = Cons (1, Cons (2, Cons (3, Cons (4, Nil))))
let my_tree = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))
let my_n_tree = NNode (1, [NLeaf, NLeaf, NLeaf])


let show_list = show my_list
let show_tree = show my_tree
let show_n_tree = show my_n_tree

let () = println show_list
let () = println show_tree
let () = println show_n_tree
