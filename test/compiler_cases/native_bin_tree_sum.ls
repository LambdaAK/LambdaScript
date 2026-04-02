Expected:
6

Source:
type rec Tree = | Leaf of int | Node of (Tree, Tree)

let rec sum_tree t =
  case t do
    | Leaf n -> n
    | Node (a, b) -> sum_tree a + sum_tree b

let () = println (int_to_str (sum_tree (Node (Leaf 1, Node (Leaf 2, Leaf 3)))))
